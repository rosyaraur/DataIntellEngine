/* ==========================================================================
   MACRO: Diallel_Analysis
   PURPOSE: Performs Griffing's Half-Diallel Analysis (GCA/SCA), extracts
            BLUPs, and generates a GCA/SCA Heatmap with marginal bar charts.
   ========================================================================== */

%MACRO Diallel_Analysis(
    data=,          /* Name of the input dataset in long format */
    p1_var=,        /* Column name for Parent 1 */
    p2_var=,        /* Column name for Parent 2 */
    rep_var=,       /* Column name for Replication/Block */
    resp_var=,      /* Column name for the Response/Trait (e.g., Yield) */
    num_parents=,   /* Total number of unique parents */
    trait_name=     /* Name of trait for plot titles (e.g., Grain Yield) */
);

    /* -------------------------------------------------------------------
       Step 1: Data Prep - Create cross ID & dummy variables for pooled GCA
       ------------------------------------------------------------------- */
    DATA _diallel_ready;
        SET &data;
        Cross = CATS(&p1_var, 'x', &p2_var);
        
        ARRAY P[&num_parents] P_1 - P_&num_parents;
        DO i = 1 TO &num_parents;
            P[i] = 0;
            IF &p1_var = i THEN P[i] = P[i] + 1;
            IF &p2_var = i THEN P[i] = P[i] + 1;
        END;
        DROP i;
    RUN;

    /* -------------------------------------------------------------------
       Step 2: Fit Mixed Model and capture BLUPs via ODS
       ------------------------------------------------------------------- */
    ODS OUTPUT SolutionR=_Random_Effects;
    PROC MIXED DATA=_diallel_ready COVTEST;
        CLASS &rep_var Cross;
        MODEL &resp_var = &rep_var / OUTP=_diallel_preds;
        RANDOM P_1 - P_&num_parents / TYPE=TOEP(1) SOLUTION; 
        RANDOM Cross / SOLUTION;
        TITLE "Diallel Analysis (&num_parents Parents) - Trait: &trait_name";
    RUN;
    QUIT;
    TITLE; /* Clear title */

    /* -------------------------------------------------------------------
       Step 3: Extract GCA & SCA Estimates
       ------------------------------------------------------------------- */
    /* Isolate GCA */
    DATA _gca_effects(KEEP=Parent GCA);
        SET _Random_Effects;
        IF SUBSTR(Effect, 1, 2) = "P_" THEN DO;
            Parent = INPUT(SUBSTR(Effect, 3), 8.); 
            GCA = Estimate;
            OUTPUT;
        END;
    RUN;

    /* Isolate SCA */
    DATA _sca_effects(KEEP=P1 P2 SCA SCA_Label);
        SET _Random_Effects;
        IF Effect = "Cross" THEN DO;
            P1 = INPUT(SCAN(Cross, 1, 'x'), 8.);
            P2 = INPUT(SCAN(Cross, 2, 'x'), 8.);
            SCA = Estimate;
            
            /* Format label with +/- signs */
            IF SCA > 0 THEN SCA_Label = "+" || STRIP(PUT(SCA, 8.1));
            ELSE IF SCA < 0 THEN SCA_Label = STRIP(PUT(SCA, 8.1));
            OUTPUT;
        END;
    RUN;

    /* -------------------------------------------------------------------
       Step 4: Build Heatmap Grid & Merge Effects
       ------------------------------------------------------------------- */
    /* Generate dynamic NxN half-grid */
    DATA _full_grid;
        DO P1 = 1 TO &num_parents;
            DO P2 = P1 TO &num_parents; 
                OUTPUT;
            END;
        END;
    RUN;

    /* Merge into plotting dataset */
    PROC SQL;
        CREATE TABLE _heatmap_data AS
        SELECT grid.P1, 
               grid.P2,
               g1.GCA AS GCA1,
               g2.GCA AS GCA2,
               s.SCA,
               s.SCA_Label
        FROM _full_grid AS grid
        LEFT JOIN _gca_effects AS g1 ON grid.P1 = g1.Parent
        LEFT JOIN _gca_effects AS g2 ON grid.P2 = g2.Parent
        LEFT JOIN _sca_effects AS s  ON grid.P1 = s.P1 AND grid.P2 = s.P2;
    QUIT;

    /* Convert numeric axes to character for GTL discrete processing */
    DATA _heatmap_data_fixed;
        SET _heatmap_data;
        P1_Char = STRIP(PUT(P1, 8.));
        P2_Char = STRIP(PUT(P2, 8.));
    RUN;

    /* -------------------------------------------------------------------
       Step 5: Define & Render the GTL Plot
       ------------------------------------------------------------------- */
    PROC TEMPLATE;
        DEFINE STATGRAPH Diallel_SCA_GCA;
            BEGINGRAPH / DESIGNWIDTH=800px DESIGNHEIGHT=700px;
                ENTRYTITLE "Combining Ability for &trait_name";
                
                RANGEATTRMAP NAME='SCAMap';
                    RANGE MIN - MAX / RANGECOLORMODEL=(CXD7191C CXFFFFBF CX1A9641);
                ENDRANGEATTRMAP;
                RANGEATTRVAR VAR=SCA ATTRVAR=SCA_Color ATTRMAP='SCAMap';

                LAYOUT LATTICE / ROWS=2 COLUMNS=2 
                                 ROWWEIGHTS=(0.20 0.80) 
                                 COLUMNWEIGHTS=(0.80 0.20)
                                 ROWDATARANGE=UNION      
                                 COLUMNDATARANGE=UNION   
                                 ROWGUTTER=5 COLUMNGUTTER=5;
                    
                    /* Top Margin: Parent 2 GCA */
                    LAYOUT OVERLAY / XAXISOPTS=(DISPLAY=NONE TYPE=DISCRETE) 
                                     YAXISOPTS=(DISPLAY=(TICKS TICKVALUES) LABEL="GCA (P2)");
                        BARCHART X=P2_Char Y=GCA2 / STAT=MEAN BARWIDTH=0.6 FILLATTRS=(COLOR=CX428BCA);
                        REFERENCELINE Y=0 / LINEATTRS=(PATTERN=SHORTDASH COLOR=BLACK);
                    ENDLAYOUT;

                    /* Top Right: Legend */
                    LAYOUT OVERLAY / BORDER=FALSE;
                        CONTINUOUSLEGEND "Heat" / TITLE="SCA Effect" ORIENT=VERTICAL;
                    ENDLAYOUT;

                    /* Center: Heatmap */
                    LAYOUT OVERLAY / XAXISOPTS=(LABEL="Parent 2" TYPE=DISCRETE) 
                                     YAXISOPTS=(LABEL="Parent 1" TYPE=DISCRETE REVERSE=TRUE);
                        HEATMAPPARM X=P2_Char Y=P1_Char COLORRESPONSE=SCA / 
                            NAME="Heat" COLORMODEL=(CXD7191C CXFFFFBF CX1A9641) OUTLINEATTRS=(COLOR=GRAY);
                        TEXTPLOT X=P2_Char Y=P1_Char TEXT=SCA_Label / TEXTATTRS=(SIZE=8pt WEIGHT=BOLD COLOR=BLACK);
                    ENDLAYOUT;

                    /* Right Margin: Parent 1 GCA */
                    LAYOUT OVERLAY / XAXISOPTS=(DISPLAY=(TICKS TICKVALUES) LABEL="GCA (P1)") 
                                     YAXISOPTS=(DISPLAY=NONE REVERSE=TRUE TYPE=DISCRETE);
                        BARCHART Y=P1_Char X=GCA1 / ORIENT=HORIZONTAL STAT=MEAN BARWIDTH=0.6 FILLATTRS=(COLOR=CX428BCA);
                        REFERENCELINE X=0 / LINEATTRS=(PATTERN=SHORTDASH COLOR=BLACK);
                    ENDLAYOUT;

                ENDLAYOUT;
            ENDGRAPH;
        END;
    RUN;

    PROC SGRENDER DATA=_heatmap_data_fixed TEMPLATE=Diallel_SCA_GCA;
    RUN;

    /* -------------------------------------------------------------------
       Step 6: Cleanup Workspace (Remove temporary datasets)
       ------------------------------------------------------------------- */
    PROC DATASETS LIBRARY=WORK NOLIST;
        DELETE _diallel_ready _Random_Effects _diallel_preds 
               _gca_effects _sca_effects _full_grid 
               _heatmap_data _heatmap_data_fixed;
    QUIT;

%MEND Diallel_Analysis;

/* Example implementation */
/* Data from Singh and Chaudary Biometrical methods */
/* 1. Load the Data in Wide Format */
DATA diallel_wide;
    INPUT P1 P2 R1 R2 R3 R4;
    DATALINES;
1 1 104.86 84.32 76.92 76.48
1 2 88.66 105.04 80.80 73.54
1 3 109.76 78.22 74.52 99.52
1 4 128.10 123.84 92.56 115.28
1 5 128.36 119.84 103.24 129.72
1 6 74.40 70.86 60.94 68.00
1 7 91.82 99.18 118.88 120.68
1 8 48.08 62.10 58.54 41.84
2 2 88.02 106.52 89.82 108.68
2 3 110.16 116.26 99.76 120.12
2 4 101.26 80.22 82.84 88.36
2 5 91.52 113.96 87.26 106.98
2 6 59.06 65.62 81.62 86.76
2 7 84.16 109.74 102.14 94.52
2 8 96.92 91.44 79.86 74.38
3 3 77.94 71.34 77.52 69.48
3 4 111.44 119.96 84.76 86.42
3 5 96.88 100.86 86.88 92.52
3 6 109.86 98.16 93.26 102.26
3 7 117.20 100.28 116.16 112.52
3 8 109.68 116.48 123.92 120.86
4 4 80.82 106.54 83.28 95.92
4 5 86.20 76.36 79.06 99.52
4 6 103.14 109.66 90.98 119.40
4 7 53.40 60.86 74.46 69.08
4 8 53.86 48.30 40.64 44.62
5 5 59.96 52.48 52.98 50.98
5 6 98.46 73.10 89.18 75.86
5 7 81.36 72.82 89.82 83.74
5 8 86.62 94.18 90.32 108.16
6 6 96.44 98.82 99.14 107.16
6 7 140.50 125.96 113.02 106.96
6 8 55.08 52.88 42.92 64.08
7 7 91.44 99.66 89.46 83.28
7 8 116.28 129.50 142.84 112.46
8 8 91.78 84.82 69.92 81.48
;
RUN;

/* 2. Transform to Long Format */
DATA diallel_long;
    SET diallel_wide;
    
    /* Create a unique cross identifier */
    Cross = CATS(P1, 'x', P2);
    
    ARRAY reps[4] R1-R4;
    DO i = 1 TO 4;
        Rep = i;
        Yield = reps[i];
        OUTPUT;
    END;
    
    DROP R1-R4 i;
RUN;

proc print;



/* Execute the all-in-one macro on the long-format dataset */
%Diallel_Analysis(
    data=diallel_long, 
    p1_var=P1, 
    p2_var=P2, 
    rep_var=Rep, 
    resp_var=Yield, 
    num_parents=8,
    trait_name=Grain Yield
);