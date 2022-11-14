sarima_sim <- function(

        # Purpose:
        #---------------------------------------------------------------------------
        # Performs a general ARIMA simulation (allowing for seasonality).
        # Convention: coefficients of all AR and MA terms in the ARIMA equation
        # have a 'negative sign', consistent with X12-ARIMA convention.
        # Returns the simulated series both as a time-series object and a
        # vector. Also generates plots of the 'integrated' and 'stationary'
        # series and other diagnostics named: "sim_plot_1.png, sim_plot_2.png" etc.
        # Note that the 'integrated' series depends strongly on the assumed
        # initial starting values for the 'undifferencing' step. A debug flag
        # allows more diagnostic results to be returned.
        #
        # By Frank Masci, Time Series Analysis Section, June 2, 2006
        # Version 3.0
        #

        # Input arguments:
        #---------------------------------------------------------------------------
        Num=100, startyear=1900, startprd=1, mean=0, var=0.001,
        nonseasonal=list(order=c(0,0,0), AR_coeffs=c(), MA_coeffs=c()),
        seasonal=list(order=c(0,0,0), AR_coeffs=c(), MA_coeffs=c(), period=1), 
        init_vals=c(), DEBUG=F

        # Num       - number of timepoints to simulate
        # startyear - integer year to assign to first timepoint
        # startprd  - integral period to assign to first timepoint; eg. 1 => JAN
        # mean      - mean level (constant) added to final undifferenced series
        # var       - innovation variance for 'rnorm' selected innovations
        # nonseasonal list:
        #     order=c(p,d,q) - non-seasonal order/differencing specification
        #     AR_coeffs      - vector of length p of (nonseasonal)AR coefficients
        #     MA_coeffs      - vector of length q of (nonseasonal MA coefficients
        # seasonal list:
        #     order=c(P,D,Q) - seasonal order/differencing specification 
        #     AR_coeffs      - vector of length P of (nonseasonal)AR coefficients
        #     MA_coeffs      - vector of length Q of (nonseasonal MA coefficients
        #     period         - seasonal periodicity; eg. 4 for qtr, 12 for monthly
        # init_vals - optional vector of length "d + D*period" initial values for
        #             undifferencing (integration) step. Default: internal
        #             initialization used.
        # DEBUG     - Boolean flag. If 'T', generates more diagnostics and plots
        #

        # Examples:
        #---------------------------------------------------------------------------
        # Simulate an Airline model series (0,1,1)(0,1,1) with periodicity 12, 300
        # timepoints starting from April 1982 and with other parameters as defined
        # by arguments above. Initial values for the undifferencing (integration)
        # step are computed from internally simulated stationary series. This call
        # returns the series as a "ts" object stored in "ts.sim":
        #
        # ts.sim <- sarima_sim( Num=300, startyear=1982, startprd=4, var=0.00026,
        #                       nonseasonal=list(order=c(0,1,1), MA_coeffs=c(0.367)),
        #                       seasonal=list(order=c(0,1,1), MA_coeffs=c(0.484),
        #                       period=12) )$ts_series;
        #
        # Same as previous example, however, the user has now provided initial values
        # for the undifferencing (integration) step from a known real-world series:
        # ie. the first "d + D*period" values of the known series are specified in
        # the "init_vals" argument vector:
        #
        # ts.sim <- sarima_sim( Num=300, startyear=1982, startprd=4, var=0.00026,
        #                       nonseasonal=list(order=c(0,1,1), MA_coeffs=c(0.367)),
        #                       seasonal=list(order=c(0,1,1), MA_coeffs=c(0.484),
        #                       period=12), init_vals=c(7.344714,7.378348,7.333149,
        #                       7.306370,7.323933,7.340343,7.355806,7.446089,7.681161,
        #                       7.321025,7.404307,7.380019,7.420955) )$ts_series;
        #
        #--------------------------------------------------------------------------- 

        ) {

        #library(fSeries);

        ##Windows:
        ## png(file="sim_plot_%d.png", width = 680, height = 450, pointsize = 12, bg="transparent", res = 200);
        ##Unix:
        ##bitmap(file="sim_plot_%d.png", width=5, height=5, res=400);

        #---------------------------------------------------------------------------
        # Define local variables from argument lists.

        p <- nonseasonal$order[1];
        d <- nonseasonal$order[2];
        q <- nonseasonal$order[3];
        AR_coeffs <- nonseasonal$AR_coeffs; 
        MA_coeffs <- nonseasonal$MA_coeffs;
        P <- seasonal$order[1];
        D <- seasonal$order[2];
        Q <- seasonal$order[3];
        SAR_coeffs <- seasonal$AR_coeffs;
        SMA_coeffs <- seasonal$MA_coeffs;
        s <- seasonal$period;
        if(DEBUG==T) {DEBUG=1} else {DEBUG=0}        

        #---------------------------------------------------------------------------
        # Check inputs and maximum allowable orders.

        if(var==0) {
          write("\n*** Input variance is zero; do you really want this? Please check; quitting...\n", file="");
          q();
        }

        if( (P != 0) || (D != 0) || (Q != 0) ) {
          if( (s != 12) && (s != 4) ) {
            write("\n*** Input period is neither 4 (quarterly) or 12 (monthly); please check; quitting...\n", file="");
            q();
          }
        }

        if(d + D > 2) {
          write("\n*** Sum of non-seasonal and seasonal differences (d + D) > 2; not recommended! Try again; quitting...\n", file="");
          q();
        }

        if(p > 3 || P > 3 || q > 3 || Q > 3) {
          write("\n*** One of the orders: p, q, P or Q is greater than 3! Such a complex model is not recommended; quitting...\n", file="");
          q();
        }

        if( p != length(AR_coeffs) ) {
          write("\n*** p-order not consistent with number of input non-seasonal AR coefficients; quitting...\n", file="");
          q();
        }

        if( P != length(SAR_coeffs) ) {
          write("\n*** P-order not consistent with number of input seasonal AR coefficients; quitting...\n", file="");
          q();
        }

        if( q != length(MA_coeffs) ) {
          write("\n*** q-order not consistent with number of input non-seasonal MA coefficients; quitting...\n", file="");
          q();
        }

        if( Q != length(SMA_coeffs) ) {
          write("\n*** Q-order not consistent with number of input seasonal MA coefficients; quitting...\n", file="");
          q();
        }

        if( Num <= 10*(p + q + P + Q) ) {
          write("\n*** Number of timepoints to simulate is less than 10 x the total number of input coefficients; simulation will be unreliable; please increase number of timepoints; quitting...\n",file="");
          q();
        }

        if( (length(init_vals) > 0) && (length(init_vals)!=(d + D*s)) ) {
          write("\n*** Length of input \"init_vals\" vector to initialise undifferencing is not equal to \"d + D*period\"; please check; quitting...\n",file="");
          q();
        }

        #---------------------------------------------------------------------------
        # Check for non-seasonal/seasonal roots within or on unit circle. Abort if so:

        if(p >= 1) {roots_AR <- armaRoots(AR_coeffs);}
        if(P >= 1) {roots_SAR <- armaRoots(SAR_coeffs);}
        if(q >= 1) {roots_MA <- armaRoots(MA_coeffs);}
        if(Q >= 1) {roots_SMA <- armaRoots(SMA_coeffs);}

        if(p >= 1) {
          for(i in 1:length(roots_AR[,3])) {
            if(roots_AR[i,3] <= 1.0) {
              write("\n*** Non-seasonal AR polynomial has a root lying on or within unit circle (see sim_plot_2.png); please try again; quitting...\n",file="");
              q();
            }
          }
        }

        if(P >= 1) {
          for(i in 1:length(roots_SAR[,3])) {
            if(roots_SAR[i,3] <= 1.0) {
              write("\n*** Seasonal AR polynomial has a root lying on or within unit circle (see sim_plot_4.png); please try again; quitting...\n",file="");
              q();
            }
          }
        }        

        if(q >= 1) {
          for(i in 1:length(roots_MA[,3])) {
            if(roots_MA[i,3] <= 1.0) {
              write("\n*** Non-seasonal MA polynomial has a root lying on or within unit circle (see sim_plot_6.png); please try again; quitting...\n",file="");
              q();
            }
          }
        }

        if(Q >= 1) {
          for(i in 1:length(roots_SMA[,3])) {
            if(roots_SMA[i,3] <= 1.0) {
              write("\n*** Seasonal MA polynomial has a root lying on or within unit circle (see sim_plot_8.png); please try again; quitting...\n",file="");
              q();
            }
          }
        }

        #---------------------------------------------------------------------------
        # Simulate stationary series vector statseries[z] (z=1:Num + buffnum) using
        # specified (seasonal) ARMA model. buffnum => to account for undifferencing later.

        # First assign coeffs to generic (3,0,3)(3,0,3)_s model below according to inputs.

        f <- c(0,0,0); # AR coeffs.
        F <- c(0,0,0); # SAR coeffs.
        t <- c(0,0,0); # MA coeffs.
        T <- c(0,0,0); # SMA coeffs.

        if(p > 0) {
          for(i in 1:length(AR_coeffs)) {
            f[[i]] = AR_coeffs[[i]];
          }
        }
        if(P > 0) {
          for(i in 1:length(SAR_coeffs)) {
            F[[i]] = SAR_coeffs[[i]];
          }
        }
        if(q > 0) {
          for(i in 1:length(MA_coeffs)) {
            t[[i]] = MA_coeffs[[i]];
          }
        }
        if(Q > 0) {
          for(i in 1:length(SMA_coeffs)) {
            T[[i]] = SMA_coeffs[[i]];
          }
        }

        # Initialise Y[tt] and e[tt] vectors for unknown timepoints
        # according to max. input model order:

        maxpoly <- max((p+P*s),(q+Q*s));

        Y <- vector();
        e <- vector();
        statseries <- vector();
        for(tt in 1:maxpoly) {
             Y[[tt]] = 0;
             e[[tt]] = 0;
        }

        buffnum <- 3*s;
       
        for( z in 1:(Num + buffnum) ) {

           tt <- z + maxpoly;
           e[[tt]] = rnorm(1, mean=0, sd=sqrt(var));

           # Ensure Y and e array indices non-zero and positive (i.e. they exist):
           if( (tt-1) <= 0 )     { g1 = 1 } else { g1 = (tt-1) }
           if( (tt-s) <= 0 )     { G1 = 1 } else { G1 = (tt-s) }
           if( (tt-1-s) <= 0 )   { g1G1 = 1 } else { g1G1 = (tt-1-s) }
           if( (tt-2) <= 0 )     { g2 = 1 } else { g2 = (tt-2) }
           if( (tt-2-s) <= 0 )   { G1g2 = 1 } else { G1g2 = (tt-2-s) }
           if( (tt-2*s) <= 0 )   { G2 = 1 } else { G2 = (tt-2*s) }
           if( (tt-1-2*s) <= 0 ) { g1G2 = 1 } else { g1G2 = (tt-1-2*s) }
           if( (tt-2-2*s) <= 0 ) { g2G2 = 1 } else { g2G2 = (tt-2-2*s) }
           if( (tt-3) <= 0 )     { g3 = 1 } else { g3 = (tt-3) }
           if( (tt-3-s) <= 0 )   { G1g3 = 1 } else { G1g3 = (tt-3-s) }
           if( (tt-3-2*s) <= 0 ) { G2g3 = 1 } else { G2g3 = (tt-3-2*s) }
           if( (tt-3*s) <= 0 )   { G3 = 1 } else { G3 = (tt-3*s) }
           if( (tt-1-3*s) <= 0 ) { g1G3 = 1 } else { g1G3 = (tt-1-3*s) }
           if( (tt-2-3*s) <= 0 ) { g2G3 = 1 } else { g2G3 = (tt-2-3*s) }
           if( (tt-3-3*s) <= 0 ) { g3G3 = 1 } else { g3G3 = (tt-3-3*s) }

           Y[[tt]] = f[[1]]*Y[[g1]] + F[[1]]*Y[[G1]] - f[[1]]*F[[1]]*Y[[g1G1]] +
                     f[[2]]*Y[[g2]] - F[[1]]*f[[2]]*Y[[G1g2]] +
                     F[[2]]*Y[[G2]] - f[[1]]*F[[2]]*Y[[g1G2]] - f[[2]]*F[[2]]*Y[[g2G2]] +
                     f[[3]]*Y[[g3]] - F[[1]]*f[[3]]*Y[[G1g3]] - F[[2]]*f[[3]]*Y[[G2g3]] +
                     F[[3]]*Y[[G3]] - f[[1]]*F[[3]]*Y[[g1G3]] - 
                     f[[2]]*F[[3]]*Y[[g2G3]] - f[[3]]*F[[3]]*Y[[g3G3]] +
                     e[[tt]] - ( t[[1]]*e[[g1]] + T[[1]]*e[[G1]] - t[[1]]*T[[1]]*e[[g1G1]] +
                     t[[2]]*e[[g2]] - T[[1]]*t[[2]]*e[[G1g2]] +
                     T[[2]]*e[[G2]] - t[[1]]*T[[2]]*e[[g1G2]] - t[[2]]*T[[2]]*e[[g2G2]] +
                     t[[3]]*e[[g3]] - T[[1]]*t[[3]]*e[[G1g3]] - T[[2]]*t[[3]]*e[[G2g3]] +
                     T[[3]]*e[[G3]] - t[[1]]*T[[3]]*e[[g1G3]] - 
                     t[[2]]*T[[3]]*e[[g2G3]] - t[[3]]*T[[3]]*e[[g3G3]] );

           statseries[[z]] = Y[[tt]];
        }

        #---------------------------------------------------------------------------
        # Undifference the series according to input differencing parameters: d and D.
        # Note constraint: d + D <= 2. Also add user-specified mean to integrated series.

        integ_series <- vector();
        IS <- vector();

        if( (d==0) && (D==0) ) {
          integ_series <- statseries + mean;
        }

        if( (d==0) && (D==1) ) {
          if( length(init_vals) > 0 ) {
            IS <- init_vals;
          } else {
            IS <- statseries[1:s];
          }
          for( z in 1:Num ) {
            tt <- z + s;
            IS[[tt]] = statseries[[tt]] + IS[[tt - s]];
            integ_series[[z]] = IS[[tt]] + mean;
          }
        }

        if( (d==0) && (D==2) ) {
          if( length(init_vals) > 0 ) {
            IS <- init_vals;
          } else {
            IS <- statseries[1:(2*s)];
          }
          for( z in 1:Num ) {
            tt <- z + 2*s;
            IS[[tt]] = statseries[[tt]] + 2*IS[[tt - s]] - IS[[tt - 2*s]];
            integ_series[[z]] = IS[[tt]] + mean;
          }
        }

        if( (d==1) && (D==0) ) {
          if( length(init_vals) > 0 ) {
            IS <- init_vals;
          } else {
            IS <- statseries[1:1];
          }
          for( z in 1:Num ) {
            tt <- z + 1;
            IS[[tt]] = statseries[[tt]] + IS[[tt - 1]];
            integ_series[[z]] = IS[[tt]] + mean;
          }
        }

        if( (d==1) && (D==1) ) {
          if( length(init_vals) > 0 ) {
            IS <- init_vals;
          } else {
            IS <- statseries[1:(1+s)];
          }
          for( z in 1:Num ) {
            tt <- z + 1 + s;
            IS[[tt]] = statseries[[tt]] + IS[[tt - 1]] + IS[[tt - s]] - IS[[tt - 1 - s]];
            integ_series[[z]] = IS[[tt]] + mean; 
          }
        }

        if( (d==2) && (D==0) ) {
          if( length(init_vals) > 0 ) {
            IS <- init_vals;
          } else {
            IS <- statseries[1:2];
          }
          for( z in 1:Num ) {
            tt <- z + 2;
            IS[[tt]] = statseries[[tt]] + 2*IS[[tt - 1]] - IS[[tt - 2]];
            integ_series[[z]] = IS[[tt]] + mean;
          }
        }

        #---------------------------------------------------------------------------
        # Convert integrated + initial stationary series vectors to time
        # series objects and plot them:

        ts_series <- ts(integ_series, start=c(startyear, startprd), frequency=s);
        ts_stationary <- ts(statseries, start=c(startyear, startprd), frequency=s);
        title1 <- paste("Simulated (integrated) Series:","(",p,d,q,")","(",P,D,Q,")","; period =",s);
        title2 <- paste("Simulated (stationary) Series:","(",p,"0",q,")","(",P,"0",Q,")","; period =",s);
        xlabel <- paste("Time ( start yr/period =",startyear,"/",startprd,")"); 
        #plot(ts_series, xlab=xlabel, ylab="Y(t)", main=title1);
        #plot(ts_stationary, xlab=xlabel, ylab="Diff_Y(t)", main=title2);

        #---------------------------------------------------------------------------
        # Debug diagnostics. Fit ARIMA model to simulated series to verify accuracy.
        # First preserve above "integ_series":

        integrated_series <- integ_series;

        if(DEBUG) {
          (fit <- arima(ts_series, order=c(p,d,q), seasonal=list(order=c(P,D,Q), period=s),
                        include.mean=TRUE) );

          acf(ts_stationary);
          pacf(ts_stationary);
          acf(ts_series);
          pacf(ts_series);

          # Sanity check: difference the integrated series to see if get same
          # stationary "statseries" simulated above.

          if( (d==0) && (D==0) ) {
            offset <- 0;
            my_stationary <- integ_series - mean;
          } else if( (d==0) && (D==1) ) {
            offset <- s;
            integ_series <- integ_series - mean;
            my_stationary <- diff(integ_series,s);
          } else if( (d==0) && (D==2) ) {
            offset <- 2*s;
            integ_series <- integ_series - mean;
            my_stationary <- diff(integ_series,s);
            my_stationary <- diff(my_stationary,s);
          } else if( (d==1) && (D==0) ) {
            offset <- 1;
            integ_series <- integ_series - mean;
            my_stationary <- diff(integ_series,1);
          } else if( (d==1) && (D==1) ) {
            offset <- 1 + s;
            integ_series <- integ_series - mean;
            my_stationary <- diff(integ_series,s);
            my_stationary <- diff(my_stationary,1);          
          } else if( (d==2) && (D==0) ) {
            offset <- 2;
            integ_series <- integ_series - mean;
            my_stationary <- diff(integ_series,1);
            my_stationary <- diff(my_stationary,1);          
          }

          # plot my initial simulated stationary series and differenced
          # integrated series to see accuracy of reconstruction:

          stationarity_check <- ts( cbind(statseries[(1+2*offset):Num+offset],
                                    my_stationary[(1+offset):length(my_stationary)]));
          # plot(stationarity_check);

        }

        #---------------------------------------------------------------------------
        # Return results.

        if(DEBUG) {
          return( list(Num=Num,var=var,d=d,D=D,p=p,P=P,q=q,Q=Q,s=s,f=f,F=F,t=t,T=T,
                       maxpoly=maxpoly,fit=fit,integrated_series=integrated_series,
                       ts_series=ts_series) );
        } else {
          return( list(integrated_series=integrated_series,ts_series=ts_series) );
        }

        dev.off();
}
