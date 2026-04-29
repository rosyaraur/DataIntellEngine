
/* ===================================================================== */
/* MACRO: GGE_Biplot_Analysis                                            */
/* INPUTS:                                                               */
/* - indata: Name of the input dataset containing the mean yields.     */
/* - gen_col: The name of the column containing Genotype labels.       */
/* NOTE: All other numeric columns will be treated as Environments.      */
/* ===================================================================== */

%macro GGE_Biplot_Analysis(indata=, gen_col=);

    /* --------------------------------------------------------- */
    /* 1. SVD and Partitioning (PROC IML)                        */
    /* --------------------------------------------------------- */
    proc iml;
        /* Read data and dynamically capture Environment column names */
        use &indata;
        read all var _NUM_ into Y_raw[colname=EnvNames]; 
        read all var {&gen_col} into Genotypes;
        close;
        
        /* Convert environment names to a column vector for the output dataset */
        Environments = EnvNames`; 
        
        /* ENVIRONMENT CENTERING: Subtract column means from the matrix */
        n_geno = nrow(Y_raw);
        col_means = Y_raw[+,] / n_geno; /* Sum each column and divide by N */
        
        /* Y_centered = Y_raw - Environment Means */
        Y_centered = Y_raw - j(n_geno, 1, 1) * col_means;

        /* Execute SVD on the centered G+GE data */
        call svd(U, Q, V, Y_centered);

        /* Isolate PC1 and PC2 and Singular Values */
        U2 = U[, 1:2];
        V2 = V[, 1:2];
        Lambda = diag(Q[1:2]); 
        Lambda_half = diag(sqrt(Q[1:2]));

        /* SCALING METHODS */
        /* A. Genotype-Focused Scaling (f = 1) */
        G_scores_f1 = U2 * Lambda;
        E_scores_f1 = V2;

        /* B. Environment-Focused Scaling (f = 0) */
        G_scores_f0 = U2;
        E_scores_f0 = V2 * Lambda;

        /* C. Symmetrical Scaling (f = 0.5) */
        G_scores_f05 = U2 * Lambda_half;
        E_scores_f05 = V2 * Lambda_half;

        /* EXPORT TO SAS DATASETS */
        Combined_f1 = (Genotypes || j(nrow(Genotypes), 1, "Genotype") || char(G_scores_f1)) // 
                      (Environments || j(nrow(Environments), 1, "Environment") || char(E_scores_f1));
        create _coords_f1 from Combined_f1[colname={"ID" "Type" "PC1" "PC2"}]; append from Combined_f1; close _coords_f1;

        Combined_f0 = (Genotypes || j(nrow(Genotypes), 1, "Genotype") || char(G_scores_f0)) // 
                      (Environments || j(nrow(Environments), 1, "Environment") || char(E_scores_f0));
        create _coords_f0 from Combined_f0[colname={"ID" "Type" "PC1" "PC2"}]; append from Combined_f0; close _coords_f0;

        Combined_f05 = (Genotypes || j(nrow(Genotypes), 1, "Genotype") || char(G_scores_f05)) // 
                       (Environments || j(nrow(Environments), 1, "Environment") || char(E_scores_f05));
        create _coords_f05 from Combined_f05[colname={"ID" "Type" "PC1" "PC2"}]; append from Combined_f05; close _coords_f05;
    quit;

    /* --------------------------------------------------------- */
    /* 2. Formatting Macro for Plotting                          */
    /* --------------------------------------------------------- */
    %macro format_plot_data(in_ds, out_ds);
        data &out_ds;
            set &in_ds;
            PC1_num = input(PC1, best12.);
            PC2_num = input(PC2, best12.);
            
            if Type = "Environment" then do;
                Orig_X = 0; Orig_Y = 0;
                Env_X = PC1_num; Env_Y = PC2_num; Env_ID = ID;
            end;
            else if Type = "Genotype" then do;
                Gen_X = PC1_num; Gen_Y = PC2_num; Gen_ID = ID;
            end;
        run;
    %mend;

    %format_plot_data(_coords_f1, _plot_f1);
    %format_plot_data(_coords_f0, _plot_f0);
    %format_plot_data(_coords_f05, _plot_f05);

    /* --------------------------------------------------------- */
    /* 3. Generate Plots                                         */
    /* --------------------------------------------------------- */
    options nobyline;

    proc sgplot data=_plot_f1 noautolegend;
        title "Genotype-Focused GGE Biplot (f = 1)";
        refline 0 / axis=x lineattrs=(pattern=shortdash color=gray);
        refline 0 / axis=y lineattrs=(pattern=shortdash color=gray);
        vector x=Env_X y=Env_Y / xorigin=Orig_X yorigin=Orig_Y lineattrs=(color=blue thickness=2) transparency=0.3;
        text x=Env_X y=Env_Y text=Env_ID / textattrs=(color=blue weight=bold);
        scatter x=Gen_X y=Gen_Y / datalabel=Gen_ID markerattrs=(symbol=circlefilled color=red size=8) datalabelattrs=(color=red);
        xaxis label="PC1" grid; yaxis label="PC2" grid;
    run;

    proc sgplot data=_plot_f0 noautolegend;
        title "Environment-Focused GGE Biplot (f = 0)";
        refline 0 / axis=x lineattrs=(pattern=shortdash color=gray);
        refline 0 / axis=y lineattrs=(pattern=shortdash color=gray);
        vector x=Env_X y=Env_Y / xorigin=Orig_X yorigin=Orig_Y lineattrs=(color=blue thickness=2) transparency=0.3;
        text x=Env_X y=Env_Y text=Env_ID / textattrs=(color=blue weight=bold);
        scatter x=Gen_X y=Gen_Y / datalabel=Gen_ID markerattrs=(symbol=circlefilled color=red size=8) datalabelattrs=(color=red);
        xaxis label="PC1" grid; yaxis label="PC2" grid;
    run;

    proc sgplot data=_plot_f05 noautolegend;
        title "Symmetrical GGE Biplot (f = 0.5)";
        refline 0 / axis=x lineattrs=(pattern=shortdash color=gray);
        refline 0 / axis=y lineattrs=(pattern=shortdash color=gray);
        vector x=Env_X y=Env_Y / xorigin=Orig_X yorigin=Orig_Y lineattrs=(color=blue thickness=2) transparency=0.3;
        text x=Env_X y=Env_Y text=Env_ID / textattrs=(color=blue weight=bold);
        scatter x=Gen_X y=Gen_Y / datalabel=Gen_ID markerattrs=(symbol=circlefilled color=red size=8) datalabelattrs=(color=red);
        xaxis label="PC1" grid; yaxis label="PC2" grid;
    run;
    
    title; /* Clear title */
    
    /* Clean up temporary macro datasets */
    proc datasets library=work nolist;
        delete _coords_f1 _coords_f0 _coords_f05 _plot_f1 _plot_f0 _plot_f05;
    quit;

%mend GGE_Biplot_Analysis;

/* How to Execute the Macro
To test it, I've transcribed the first 5 rows of Table 4.4 from your image into a DATA step. 
You simply load the data, compile the macro above, and then call the macro with one line of
 code */
/* Create test dataset matching Table 4.4 format */
data wheat_trials;
    input Names $ BH93 EA93 HW93 ID93 KE93 NN93 OA93 RN93 WP93;
    datalines;
Ann 4.460 4.150 2.849 3.084 5.940 4.450 4.351 4.039 2.672
Ari 4.417 4.771 2.912 3.506 5.699 5.152 4.956 4.386 2.938
Aug 4.669 4.578 3.098 3.460 6.070 5.025 4.730 3.900 2.621
Cas 4.732 4.745 3.375 3.904 6.224 5.340 4.226 4.893 3.451
Del 4.390 4.603 3.511 3.848 5.773 5.421 5.147 4.098 2.832
;
run;

/* Call the macro! */
%GGE_Biplot_Analysis(indata=wheat_trials, gen_col=Names);


