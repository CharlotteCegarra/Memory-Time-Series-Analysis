/* Import Data */
proc import datafile="C:\Users\daddi\OneDrive\Bureau\BD_indice boursier.csv"
    out=snp500
    replace;
run;

proc sgplot data=snp500;
    title "Share price evolution";
    series x=date y=spx / markers lineattrs=(color=blue thickness=2);
    xaxis label="Date" type=time interval=month;
    yaxis label="Share Price" grid;
    format date date9.;
run;

/* Filter Dates */
data snp500;
    set snp500;
    if '01OCT2002'd <= date <= '15NOV2007'd;
    format date date9.;
run;

/* Plot Evolution of Price */
proc sgplot data=snp500;
    title "Share price evolution";
    series x=date y=spx / markers lineattrs=(color=blue thickness=2);
    xaxis label="Date" type=time interval=month;
    yaxis label="Share Price" grid;
    format date date9.;
run;

/* Data Transformation */
data snp500;
    set snp500;
    logsp500 = log(spx);
    keep date logsp500;
run;

/* IML for Denoising and Correlation Calculation */
proc iml;
    /* Definition of G function */
    start G(x);
        g = 1/(sqrt(2*constant("pi")*10)) * exp(-x##2 / (2*10##2));
        return(g);
    finish G;

    /* Import data into matrix LX */
    use snp500;
    read all var {date logsp500} into LX[colname={date logsp500}];
    
    /* Initialize denoised SP500 matrix */
    denoise_sp500 = j(nrow(LX), 2, 0);
    denoise_sp500[,1] = LX[,1]; /* Assign dates */
    w = G((0:nrow(LX)-1)`);
    
    /* Apply moving average filter */
    do j = 1 to nrow(LX);
        if j > 1 then
            ma = sum(w[1:j-1]#LX[1:j-1, 2]) / sum(w[1:j-1]);
        else
            ma = LX[j, 2];
        denoise_sp500[j, 2] = LX[j, 2] - ma;
    end;

    /* Create denoise_sp500 table */
    create denoisesp500 from denoise_sp500[colname={"date", "denoise"}];
    append from denoise_sp500;
    close denoisesp500;

    /* Calculate correlations */
    use denoisesp500;
    read all var {denoise} into D;
    T = nrow(D);
    w = int(T/2); /* Use half of the total data length for correlation calculation */
    res = j(T-w, 4, .);
    do i = 1 to T-w;
        x1 = D[i:w+i-1];
        x2 = lag(x1)||x1;
        c = corr(x2);
        res[i,] = LX[w+i-1, 1] || c[1,2] || var(x1) || skewness(x1); /* Correct date assignment */
    end;

    /* Create result table */
    create Table_sp500 from res[colname={"date", "autocorrelation", "var", "skewness"}];
    append from res;
    close Table_sp500;
quit;

/* Plot Results */
proc sgplot data=Table_sp500;
    title "Variance over Time";
    series x=date y=var / markers lineattrs=(color=red thickness=2);
    xaxis label="Date" type=time interval=month;
    yaxis label="Variance" grid;
    format date date9.;
run;

proc sgplot data=Table_sp500;
    title "Skewness over time";
    series x=date y=skewness / markers lineattrs=(color=green thickness=2);
    xaxis label="Date" type=time interval=month;
    yaxis label="Skewness" grid;
    format date date9.;
run;

proc sgplot data=Table_sp500;
    title "Autocorrelation Over Time";
    series x=date y=autocorrelation / markers lineattrs=(color=blue thickness=2);
    xaxis label="Date" type=time interval=month;
    yaxis label="Autocorrelation" grid;
    format date date9.;
run;













/* Bitcoin Data Import */
proc import datafile="C:\Users\daddi\OneDrive\Bureau\BTC-USD.csv"
    out=bitcoin
    replace;
run;

/* Plot Evolution of Price */
proc sgplot data=bitcoin;
    title "Evolution of the Bitcoin Price";
    series x=date y=open / markers lineattrs=(color=blue thickness=2);
    xaxis label="Date" type=time interval=month;
    yaxis label="Opening price" grid;
    format date date9.;
run;

/* Filter Dates */
data bitcoin;
    set bitcoin;
    if '01MAR2017'd <= date <= '20DEC2017'd;
    format date date9.;
run;

/* Plot Evolution of Price */
proc sgplot data=bitcoin;
    title "Evolution of the Bitcoin Price";
    series x=date y=open / markers lineattrs=(color=blue thickness=2);
    xaxis label="Date" type=time interval=month;
    yaxis label="Opening price" grid;
    format date date9.;
run;

/* Data Transformation */
data bitcoin;
    set bitcoin;
    logopen = log(open);
    keep date logopen;
run;


/* IML for Denoising and Correlation Calculation */
proc iml;
    /* Definition of G function */
    start G(x);
        g = 1/(sqrt(2*3.14)*10) * exp(-x##2 / (2*10##2));
        return(g);
    finish G;

    /* Import data into matrix LX */
    use bitcoin;
    read all var {date logopen} into LX[colname={date logopen}];
    
    /* Initialize denoised bitcoin matrix */
    denoise_bitcoin = j(nrow(LX), 2, 0);
    denoise_bitcoin[,1] = LX[,1]; /* Assign dates */
    w = G((0:nrow(LX)-1)`);
    
    /* Apply moving average filter */
    do j = 1 to nrow(LX);
        if j > 1 then
            ma = sum(w[1:j-1]#LX[1:j-1, 2]) / sum(w[1:j-1]);
        else
            ma = LX[j, 2];
        denoise_bitcoin[j, 2] = LX[j, 2] - ma;
    end;

    /* Create denoisebitcoin table */
    create denoisebitcoin from denoise_bitcoin[colname = {"date", "denoise"}];
    append from denoise_bitcoin;
    close denoisebitcoin;

    /* Calculate correlations */
    use denoisebitcoin;
    read all var {denoise} into D;
    T = nrow(D);
    w = int(T/5);
    res = j(T-w, 4, .);
    do i = 1 to T-w;
        x1 = D[i:w+i-1];
        x2 = lag(x1)||x1;
        c = corr(x2);
        res[i,] = LX[w+i-1, 1] || c[1,2] || var(x1) || skewness(x1); /* Correct date assignment */
    end;

    /* Create result table */
    create Table_bitcoin from res[colname = {"date", "autocorrelation", "var", "skewness"}];
    append from res;
    close Table_bitcoin;
quit;

/* Plot Results */
proc sgplot data=Table_bitcoin;
    title "Variance over Time";
    series x=date y=var / markers lineattrs=(color=red thickness=2);
    xaxis label="Date" type=time interval=month;
    yaxis label="Variance" grid;
    format date date9.;
run;

proc sgplot data=Table_bitcoin;
    title "Skewness over time";
    series x=date y=skewness / markers lineattrs=(color=green thickness=2);
    xaxis label="Date" type=time interval=month;
    yaxis label="Skewness" grid;
    format date date9.;
run;

proc sgplot data=Table_bitcoin;
    title "Autocorrelation Over Time";
    series x=date y=autocorrelation / markers lineattrs=(color=blue thickness=2);
    xaxis label="Date" type=time interval=month;
    yaxis label="Autocorrelation" grid;
    format date date9.;
run;








/* Generation of Synthetic Meteorological Data */
data meteorological;
    format date date9.;
    do date = '01JAN2020'd to '31DEC2022'd;
        
        temperature = 15 + 10 * sin(2 * constant('PI') * (date - '01JAN2020'd) / 365.25 - constant('PI') / 2) + rannor(123);

        
        if '15JUN2021'd <= date <= '15JUL2021'd then temperature = temperature - 15 * (date - '15JUN2021'd) / ('15JUL2021'd - '15JUN2021'd);
        else if '15JUL2021'd < date <= '15AUG2021'd then temperature = temperature - 15 * ('15AUG2021'd - date) / ('15AUG2021'd - '15JUL2021'd);
        
        output;
    end;
run;

proc sgplot data=meteorological;
    title "Evolution of Temperature";
    series x=date y=temperature / markers lineattrs=(color=blue thickness=2);
    xaxis label="Date" type=time interval=month;
    yaxis label="Temperature (°C)" grid;
    format date date9.;
run;

/* Filter Dates */
data meteorological;
    set meteorological;
    if '01JAN2021'd <= date <= '14JUN2021'd;
run;

/* Visualization of Temperature Evolution */
proc sgplot data=meteorological;
    title "Evolution of Temperature";
    series x=date y=temperature / markers lineattrs=(color=blue thickness=2);
    xaxis label="Date" type=time interval=month;
    yaxis label="Temperature (°C)" grid;
    format date date9.;
run;

/* Data Transformation */
data meteorological;
    set meteorological;
    logtemp = log(temperature);
    keep date logtemp;
run;

/* IML for Denoising and Correlation Calculation */
proc iml;
    /* Definition of G function */
    start G(x);
        g = 1/(sqrt(2*3.14)*10) * exp(-x##2 / (2*10##2));
        return(g);
    finish G;

    /* Import data into matrix LX */
    use meteorological;
    read all var {date logtemp} into LX[colname={date logtemp}];
    
    /* Initialize denoised meteorological matrix */
    denoise_meteorological = j(nrow(LX), 2, 0);
    denoise_meteorological[,1] = LX[,1]; /* Assign dates */
    w = G((0:nrow(LX)-1)`);
    
    /* Apply moving average filter */
    do j = 1 to nrow(LX);
        if j > 1 then
            ma = sum(w[1:j-1]#LX[1:j-1, 2]) / sum(w[1:j-1]);
        else
            ma = LX[j, 2];
        denoise_meteorological[j, 2] = LX[j, 2] - ma;
    end;

    /* Create denoise_meteorological table */
    create denoisemeteorological from denoise_meteorological[colname = {"date", "denoise"}];
    append from denoise_meteorological;
    close denoisemeteorological;

    /* Calculate correlations */
    use denoisemeteorological;
    read all var {denoise} into D;
    T = nrow(D);
    w = int(T/5);
    res = j(T-w, 4, .);
    do i = 1 to T-w;
        x1 = D[i:w+i-1];
        x2 = lag(x1)||x1;
        c = corr(x2);
        res[i,] = LX[w+i-1, 1] || c[1,2] || var(x1) || skewness(x1); /* Correct date assignment */
    end;

    /* Create result table */
    create Table_meteorological from res[colname = {"date", "autocorrelation", "var", "skewness"}];
    append from res;
    close Table_meteorological;
quit;

/* Plot Results */
proc sgplot data=Table_meteorological;
    title "Variance over Time";
    series x=date y=var / markers lineattrs=(color=red thickness=2);
    xaxis label="Date" type=time interval=month;
    yaxis label="Variance" grid;
    format date date9.;
run;

proc sgplot data=Table_meteorological;
    title "Skewness over time";
    series x=date y=skewness / markers lineattrs=(color=green thickness=2);
    xaxis label="Date" type=time interval=month;
    yaxis label="Skewness" grid;
    format date date9.;
run;

proc sgplot data=Table_meteorological;
    title "Autocorrelation Over Time";
    series x=date y=autocorrelation / markers lineattrs=(color=blue thickness=2);
    xaxis label="Date" type=time interval=month;
    yaxis label="Autocorrelation" grid;
    format date date9.;
run;

