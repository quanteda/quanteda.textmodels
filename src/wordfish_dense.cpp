#include "lib.h"
#include <math.h>
using namespace Rcpp;
using namespace arma;


// [[Rcpp::export]]
Rcpp::List qatd_cpp_wordfish_dense(SEXP wfm, SEXP dir, SEXP priors, SEXP tol, SEXP disp, SEXP dispfloor, bool abs_err){

    // DEFINE INPUTS

    Rcpp::NumericMatrix Y(wfm);
    Rcpp::NumericVector priorvec(priors);
    Rcpp::NumericVector tolvec(tol);
    Rcpp::IntegerVector dirvec(dir);
    Rcpp::IntegerVector disptype(disp);
    Rcpp::NumericVector dispmin(dispfloor);

    double priorprecalpha = priorvec(0);
    double priorprecpsi = priorvec(1);
    double priorprecbeta = priorvec(2);
    double priorprectheta = priorvec(3);
    int method = priorvec(4);
    double zeta = 1.0;

    int N = Y.nrow();
    int K = Y.ncol();

    // SET INITIAL VALUES
    Rcpp::NumericVector alpha(N);
    Rcpp::NumericVector psi(K);
    Rcpp::NumericVector beta(K);
    Rcpp::NumericVector theta(N);
    Rcpp::NumericVector beta2(K);
    Rcpp::NumericVector theta2(N);

    Rcpp::NumericVector thetaSE(N); // document position standard errors
    Rcpp::NumericVector theta2SE(N); // document position standard errors
    Rcpp::NumericVector alphaSE(N); // document position standard errors
    Rcpp::NumericVector psiSE(K); // document position standard errors
    Rcpp::NumericVector betaSE(K); // document position standard errors
    Rcpp::NumericVector beta2SE(K); // document position standard errors
    Rcpp::NumericVector phi(K,1.0); // word-level dispersion parameters

    // Construct Chi-Sq Residuals
    arma::mat C(Y.begin(), N, K);
    arma::colvec rsum = sum(C,1);
    arma::rowvec csum = sum(C,0);
    double asum = sum(rsum);
    for (int i = 0; i < N; i++){
        for (int k=0; k < K; k++){
            C(i,k) = (Y(i,k) - rsum(i)*csum(k)/asum)/sqrt(rsum(i)*csum(k)/asum);
        }
    }

    // Singular Value Decomposition of Chi-Sq Residuals
    arma::mat U(N,N);
    arma::vec s(N);
    arma::mat V(K,N);
    svd(U,s,V,C);

    // Load initial values
    for (int i = 0; i < N; i++) {
        theta(i) = pow(rsum(i) / asum,-0.5) * U(i,0);
        theta2(i) = 0;
        //Rcout<<"theta starting values:"<<theta(i)<<std::endl;
    }
    for (int k=0; k < K; k++){
      beta(k) = -0.5; // pow(csum(k)/asum,-0.5) * V(k,0);
      beta2(k) = 0;
    } 
    for (int i=0; i < N; i++) alpha = log(rsum);
    psi = log(csum/N);

    alpha = alpha - log(mean(rsum));
    theta = (theta - mean(theta)) / sd(theta);

    // Create temporary variables
    Rcpp::NumericMatrix pars(2,1);
    Rcpp::NumericMatrix newpars(2,1);
    Rcpp::NumericMatrix G(2,1);
    Rcpp::NumericMatrix H(2,2);
    double loglambdaik;
    double lambdaik;
    double mutmp;
    double phitmp;
    double zetatmp;
    Rcpp::NumericVector lambdai(K);
    Rcpp::NumericVector lambdak(N);
    double stepsize = 1.0;
    double cc = 0.0;
    double l_mean = 0.0;
    double l2_mean = 0.0;
    double gg = 0;
    double hh = 0;
    int inneriter = 0;
    int outeriter = 0;

    double lastlp = -2000000000000.0;
    double lp = -1.0 * (sum(0.5 * ((alpha*alpha) * (priorprecalpha))) +
                        sum(0.5 * ((psi * psi) * (priorprecpsi))) +
                        sum(0.5 * ((beta*beta)*(priorprecbeta))) +
                        sum(0.5 * ((theta*theta)*(priorprectheta))));
    // double lp_P = lp;
    
    for (int i = 0; i < N; i++){
      for (int k = 0; k < K; k++){
        loglambdaik = alpha(i) + psi(k) + beta(k) * theta(i);
        lp = lp + loglambdaik * Y(i,k) - exp(loglambdaik);
      }
    }

    // BEGIN WHILE LOOP
    double err = (abs_err == true) ? fabs(lp - lastlp) : (lp - lastlp);
    while ((err > tolvec(0)) && outeriter < 100) {
        outeriter++;

        // UPDATE WORD PARAMETERS
        for (int k = 0; k < K; k++) {
            cc = 1;
            inneriter = 0;
            if (outeriter == 1) stepsize = 0.5;
            while ((cc > tolvec(1)) && inneriter < 10){
              inneriter++;
              lambdak = exp(alpha + psi(k) + beta(k) * theta);
              G(0,0) = sum(Y(_,k) - lambdak) / phi(k) - psi(k) * (priorprecpsi);
              G(1,0) = sum(theta * (Y(_,k) - lambdak)) / phi(k) - beta(k) * (priorprecbeta);
              H(0,0) = -sum(lambdak) / phi(k) - priorprecpsi;
              H(1,0) = -sum(theta * lambdak)/phi(k);
              H(0,1) = H(1,0);
              H(1,1) = -sum((theta * theta) * lambdak) / phi(k) - priorprecbeta;
              pars(0,0) = psi(k);
              pars(1,0) = beta(k);
              newpars(0,0) = pars(0,0) - stepsize * (H(1,1) * G(0,0) - H(0,1) * G(1,0)) / (H(0,0) * H(1,1) - H(0,1) * H(1,0));
              newpars(1,0) = pars(1,0) - stepsize * (H(0,0) * G(1,0) - H(1,0) * G(0,0)) / (H(0,0) * H(1,1) - H(0,1) * H(1,0));
              psi(k) = newpars(0,0);
              beta(k) = newpars(1,0);
              cc = max(abs(newpars - pars));
              stepsize = 1.0;
            }
        }


        // UPDATE DOCUMENT PARAMETERS
        for (int i = 0; i < N; i++){
            cc = 1;
            inneriter = 0;
            if (outeriter == 1) stepsize = 0.5;
            while ((cc > tolvec(1)) && inneriter < 10){
              inneriter++;
              lambdai = exp(alpha(i) + psi + beta * theta(i));
              G(0,0) = sum((Y(i,_) - lambdai) / phi) - alpha(i) * priorprecalpha;
              G(1,0) = sum((beta * (Y(i,_) - lambdai)) / phi) - theta(i) * priorprectheta;
              H(0,0) = -sum(lambdai/phi) - priorprecalpha;
              H(1,0) = -sum((beta * lambdai) / phi);
              H(0,1) = H(1,0);
              H(1,1) = -sum(((beta * beta) * lambdai) / phi) - priorprectheta;
              pars(0,0) = alpha(i);
              pars(1,0) = theta(i);
              newpars(0,0) = pars(0,0) - stepsize * (H(1,1) * G(0,0) - H(0,1) * G(1,0)) / (H(0,0) * H(1,1) - H(0,1) * H(1,0));
              newpars(1,0) = pars(1,0) - stepsize * (H(0,0) * G(1,0) - H(1,0) * G(0,0)) / (H(0,0) * H(1,1) - H(0,1) * H(1,0));
              alpha(i) = newpars(0,0);
              theta(i) = newpars(1,0);
              cc = max(abs(newpars - pars));
              stepsize = 1.0;
            }
        }

        // UPDATE DISPERSION PARAMETERS

        if (disptype(0) == 2) { // single dispersion parameter for all words
            phitmp = 0.0;
            for (int k = 0; k < K; k++){
                for (int i=0; i < N; i++){
                    mutmp = exp(alpha(i) + psi(k) + beta(k) * theta(i));
                    phitmp = phitmp + (Y(i,k) - mutmp) * (Y(i,k) - mutmp) / mutmp;
                }
            }
            phitmp = phitmp / (N * K - 2 * N - 2 * K);
            for (int k = 0; k < K; k++) phi(k) = phitmp;
        }

        if (disptype(0) >= 3) { // individual dispersion parameter for each word
            for (int k = 0; k < K; k++){
                phitmp = 0.0;
                for (int i = 0; i < N; i++){
                    mutmp = exp(alpha(i) + psi(k) + beta(k) * theta(i));
                    phitmp = phitmp + (Y(i,k) - mutmp) * (Y(i,k) - mutmp) / mutmp;
                }
                phitmp = (K * phitmp) / (N * K - 2 * N - 2 * K);
                phi(k) = phitmp;
                // set ceiling on underdispersion
                if (disptype(0) == 4) phi(k) = fmax(dispmin(0), phi(k));
            }
        }

        alpha = alpha - mean(alpha);
        theta = (theta - mean(theta))/sd(theta);

        // CHECK LOG-POSTERIOR FOR CONVERGENCE
        lastlp = lp;
        lp = -1.0 * (sum(0.5 * ((alpha*alpha) * (priorprecalpha))) + sum(0.5 * ((psi*psi) * (priorprecpsi))) + sum(0.5 * ((beta*beta)*(priorprecbeta))) + sum(0.5 * ((theta*theta)*(priorprectheta))));
        for (int i = 0; i < N; i++){
            for (int k = 0; k < K; k++){
                loglambdaik = alpha(i) + psi(k) + beta(k) * theta(i);
                lp = lp + loglambdaik * Y(i,k) - exp(loglambdaik);
            }
        }
        // Rprintf("%d: %f2\\n",outeriter,lp);
        //Rcout<<"outeriter="<<outeriter<<"  lp - lastlp= "<<lp - lastlp<<std::endl;
        err = (abs_err == true) ? fabs(lp - lastlp) : (lp - lastlp);
        // END WHILE LOOP
        
    }
    // lp = lp_P;
    
    // IF TWO-DIMENSIONAL POISSON
    if (method == 4){
      // CALCULATE THE START VALUES OF THETA_2 AND BETA_2
      // Construct Chi-Sq Residuals
      for (int i = 0; i < N; i++){
        for (int k=0; k < K; k++){
          lambdaik = exp(alpha(i) + psi(k) + beta(k) * theta(i));
          C(i,k) = Y(i,k) - lambdaik;
        }
      }
      
      // Singular Value Decomposition of Chi-Sq Residuals
      svd(U,s,V,C);
      
      // Load initial values
      for (int i = 0; i < N; i++) {
        theta2(i) = U(i,0);
        //Rcout<<"theta starting values:"<<theta(i)<<std::endl;
      }
      for (int k=0; k < K; k++){
        beta2(k) = V(k,0);
      }
      
      // cout << "theta2: " << theta2 << endl;
      //
      // cout << "beta2: " << beta2 << endl;
      
      
      gg = 0;
      hh = 0;
      cc = 0.0;
      inneriter = 0;
      outeriter = 0;
      lastlp = -2000000000000.0;
      lp = lp - sum(0.5 * ((beta2*beta2)*(priorprecbeta))) + sum(0.5 * ((theta2*theta2)*(priorprectheta)));
      err = (abs_err == true) ? fabs(lp - lastlp) : (lp - lastlp);
      while ((err > tolvec(0)) && outeriter < 100) {
        outeriter++;
        
        // UPDATE WORD PARAMETERS
        for (int k = 0; k < K; k++) {
          cc = 1;
          inneriter = 0;
          if (outeriter == 1) stepsize = 0.5;
          while ((cc > tolvec(1)) && inneriter < 10){
            inneriter++;
            lambdak = exp(alpha + psi(k) + beta(k) * theta + beta2(k) * theta2);
            gg = sum(theta2 * (Y(_,k) - lambdak)) / phi(k) - beta2(k) * (priorprecbeta);
            hh = -sum((theta2 * theta2) * lambdak) / phi(k) - priorprecbeta;
            pars(0,0) = beta2(k);
            newpars(0,0) = pars(0,0) - stepsize * gg / hh;
            beta2(k) = newpars(0,0);
            cc = max(abs(newpars - pars));
            stepsize = 1.0;
          }
        }
        
        
        // UPDATE DOCUMENT PARAMETERS
        for (int i = 0; i < N; i++){
          cc = 1;
          inneriter = 0;
          if (outeriter == 1) stepsize = 0.5;
          while ((cc > tolvec(1)) && inneriter < 10){
            inneriter++;
            lambdai = exp(alpha(i) + psi + beta * theta(i));
            gg = sum(beta2 * (Y(i,_) - lambdai) / phi) - theta2(i) * (priorprectheta);
            hh = -sum((beta2 * beta2) * lambdai / phi) - priorprectheta;
            pars(0,0) = theta2(i);
            newpars(0,0) = pars(0,0) - stepsize * gg / hh;
            theta2(i) = newpars(0,0);
            cc = max(abs(newpars - pars));
            stepsize = 1.0;
          }
        }
        
        // UPDATE DISPERSION PARAMETERS
        
        if (disptype(0) == 2) { // single dispersion parameter for all words
          phitmp = 0.0;
          for (int k = 0; k < K; k++){
            for (int i=0; i < N; i++){
              mutmp = exp(alpha(i) + psi(k) + beta(k) * theta(i) + beta2(k) * theta2(i));
              phitmp = phitmp + (Y(i,k) - mutmp) * (Y(i,k) - mutmp) / mutmp;
            }
          }
          phitmp = phitmp / (N * K - 2 * N - 2 * K);
          for (int k = 0; k < K; k++) phi(k) = phitmp;
        }
        
        if (disptype(0) >= 3) { // individual dispersion parameter for each word
          for (int k = 0; k < K; k++){
            phitmp = 0.0;
            for (int i = 0; i < N; i++){
              mutmp = exp(alpha(i) + psi(k) + beta(k) * theta(i) + beta2(k) * theta2(i));
              phitmp = phitmp + (Y(i,k) - mutmp) * (Y(i,k) - mutmp) / mutmp;
            }
            phitmp = (K * phitmp) / (N * K - 2 * N - 2 * K);
            phi(k) = phitmp;
            // set ceiling on underdispersion
            if (disptype(0) == 4) phi(k) = fmax(dispmin(0), phi(k));
          }
        }
        
        theta2 = (theta2 - mean(theta2))/sd(theta2);
        
        // CHECK LOG-POSTERIOR FOR CONVERGENCE
        lastlp = lp;
        lp = -1.0 * (sum(0.5 * ((alpha*alpha) * (priorprecalpha))) + sum(0.5 * ((psi*psi) * (priorprecpsi))) +
          sum(0.5 * ((beta*beta)*(priorprecbeta))) + sum(0.5 * ((theta*theta)*(priorprectheta))) +
          sum(0.5 * ((beta2*beta2)*(priorprecbeta))) + sum(0.5 * ((theta2*theta2)*(priorprectheta))));
        for (int i = 0; i < N; i++){
          for (int k = 0; k < K; k++){
            loglambdaik = alpha(i) + psi(k) + beta(k) * theta(i) + beta2(k) * theta2(i);
            lp = lp + loglambdaik * Y(i,k) - exp(loglambdaik);
          }
        }
        // Rprintf("%d: %f2\\n",outeriter,lp);
        //Rcout<<"outeriter="<<outeriter<<"  lp - lastlp= "<<lp - lastlp<<std::endl;
        err = (abs_err == true) ? fabs(lp - lastlp) : (lp - lastlp);
        // END WHILE LOOP
        
      }
    }


    // cout << "skipped?" << endl;
    
    // IF USE THE NB MODEL
    if (method == 99 || method == 10000){
      cc = 0.0;
      inneriter = 0;
      outeriter = 0;
      lastlp = -2000000000000.0;
      lp = -1.0 * (sum(0.5 * ((alpha*alpha) * (priorprecalpha))) +
                   sum(0.5 * ((psi * psi) * (priorprecpsi))) +
                   sum(0.5 * ((beta*beta)*(priorprecbeta))) +
                   sum(0.5 * ((theta*theta)*(priorprectheta))));
      // double lp_NB = lp;
      
      for (int i = 0; i < N; i++){
        for (int k = 0; k < K; k++){
          loglambdaik = alpha(i) + psi(k) + beta(k) * theta(i);
          lp = lp + loglambdaik * Y(i,k) - (log(exp(loglambdaik)+zeta)) * (Y(i,k) + zeta);
        }
      }
      // BEGIN WHILE LOOP
      double err = (abs_err == true) ? fabs(lp - lastlp) : (lp - lastlp);
      while ((err > tolvec(0)) && outeriter < 100) {
        outeriter++;
        
        // UPDATE WORD PARAMETERS
        for (int k = 0; k < K; k++) {
          cc = 1;
          inneriter = 0;
          if (outeriter == 1) stepsize = 0.5;
  
          while ((cc > tolvec(1)) && inneriter < 10){
            inneriter++;
            lambdak = exp(alpha + psi(k) + beta(k) * theta);
            G(0,0) = sum(zeta * (Y(_,k) - lambdak)/(lambdak + zeta)) - psi(k) * (priorprecpsi);
            G(1,0) = sum(theta * zeta * (Y(_,k) - lambdak)/(lambdak + zeta)) - beta(k) * (priorprecbeta);
            H(0,0) = -sum(zeta * lambdak * (Y(_,k) + zeta)/((lambdak + zeta) * (lambdak + zeta))) - priorprecpsi;
            H(1,0) = -sum(zeta * lambdak * theta * (Y(_,k) + zeta)/((lambdak + zeta) * (lambdak + zeta)));
            H(0,1) = H(1,0);
            H(1,1) = -sum(zeta * lambdak * theta * theta * (Y(_,k) + zeta)/((lambdak + zeta) * (lambdak + zeta))) - priorprectheta;
            pars(0,0) = psi(k);
            pars(1,0) = beta(k);
            newpars(0,0) = pars(0,0) - stepsize * (H(1,1) * G(0,0) - H(0,1) * G(1,0)) / (H(0,0) * H(1,1) - H(0,1) * H(1,0));
            newpars(1,0) = pars(1,0) - stepsize * (H(0,0) * G(1,0) - H(1,0) * G(0,0)) / (H(0,0) * H(1,1) - H(0,1) * H(1,0));
            psi(k) = newpars(0,0);
            beta(k) = newpars(1,0);
            cc = max(abs(newpars - pars));
            stepsize = 1;
          }
        }

  
        // UPDATE DOCUMENT PARAMETERS
        for (int i = 0; i < N; i++){
          cc = 1;
          inneriter = 0;
          if (outeriter == 1) stepsize = 0.5;
          while ((cc > tolvec(1)) && inneriter < 10){
            inneriter++;
            lambdai = exp(alpha(i) + psi + beta * theta(i));
            G(0,0) = sum(zeta * (Y(i,_) - lambdai)/(lambdai + zeta)) - alpha(i) * priorprecalpha;
            G(1,0) = sum(beta * zeta * (Y(i,_) - lambdai)/(lambdai + zeta)) - theta(i) * priorprectheta;
            H(0,0) = -sum(zeta * lambdai * (Y(i,_) + zeta)/((lambdai + zeta) * (lambdai + zeta))) - priorprecalpha;
            H(1,0) = -sum(zeta * lambdai * beta * (Y(i,_) + zeta)/((lambdai + zeta) * (lambdai + zeta)));
            H(0,1) = H(1,0);
            H(1,1) = -sum(zeta * lambdai * beta * beta * (Y(i,_) + zeta)/((lambdai + zeta) * (lambdai + zeta))) - priorprectheta;
            pars(0,0) = alpha(i);
            pars(1,0) = theta(i);
            newpars(0,0) = pars(0,0) - stepsize * (H(1,1) * G(0,0) - H(0,1) * G(1,0)) / (H(0,0) * H(1,1) - H(0,1) * H(1,0));
            newpars(1,0) = pars(1,0) - stepsize * (H(0,0) * G(1,0) - H(1,0) * G(0,0)) / (H(0,0) * H(1,1) - H(0,1) * H(1,0));
            alpha(i) = newpars(0,0);
            theta(i) = newpars(1,0);
            cc = max(abs(newpars - pars));
            stepsize = 1;
          }
        }

        // UPDATE DISPERSION PARAMETERS
        zetatmp = 0.0;
        l_mean = 0.0;
        l2_mean = 0.0;
        for (int k = 0; k < K; k++){
          for (int i=0; i < N; i++){
            mutmp = exp(alpha(i) + psi(k) + beta(k) * theta(i));
            l_mean = l_mean + mutmp;
            l2_mean = l2_mean + mutmp * mutmp;
            // zetatmp = zetatmp + mutmp * mutmp / ((Y(i,k) - mutmp) * (Y(i,k) - mutmp) - mutmp);
            // zetatmp = zetatmp + (Y(i,k) - mutmp) * (Y(i,k) - mutmp);
          }
        }
        l_mean = l_mean / (N*K);
        l2_mean = l2_mean / (N*K);
        
        
        for (int k = 0; k < K; k++){
          for (int i=0; i < N; i++){
            zetatmp = zetatmp + (Y(i,k) - l_mean) * (Y(i,k) - l_mean);
          }
        }
        
        // zeta = zetatmp;
        zeta = l2_mean / ((zetatmp / (N*K-1)) - l_mean);
        
        alpha = alpha - mean(alpha);
        theta = (theta - mean(theta))/sd(theta);
        
        // CHECK LOG-POSTERIOR FOR CONVERGENCE
        lastlp = lp;
        lp = -1.0 * (sum(0.5 * ((alpha*alpha) * (priorprecalpha))) + sum(0.5 * ((psi*psi) * (priorprecpsi))) + sum(0.5 * ((beta*beta)*(priorprecbeta))) + sum(0.5 * ((theta*theta)*(priorprectheta))));
        for (int i = 0; i < N; i++){
          for (int k = 0; k < K; k++){
            loglambdaik = alpha(i) + psi(k) + beta(k) * theta(i);
            lp = lp + loglambdaik * Y(i,k) - (log(exp(loglambdaik)+zeta)) * (Y(i,k) + zeta);
          }
        }
        
        // Rprintf("%d: %f2\\n",outeriter,lp);
        //Rcout<<"outeriter="<<outeriter<<"  lp - lastlp= "<<lp - lastlp<<std::endl;
        err = (abs_err == true) ? fabs(lp - lastlp) : (lp - lastlp);
        // END WHILE LOOP
      }
      // END NB MODEL
      // lp = lp_NB;
      // IF TWO-DIMENSIONAL NB

      if (method == 10000){
        for (int i = 0; i < N; i++){
          for (int k=0; k < K; k++){
            lambdaik = exp(alpha(i) + psi(k) + beta(k) * theta(i));
            C(i,k) = Y(i,k) - lambdaik;
          }
        }

        
        // Singular Value Decomposition of Chi-Sq Residuals
        svd(U,s,V,C);
        
        // Load initial values
        for (int i = 0; i < N; i++) {
          theta2(i) = U(i,0);
          //Rcout<<"theta starting values:"<<theta(i)<<std::endl;
        }
        for (int k=0; k < K; k++){
          beta2(k) = V(k,0);
        }
        
        
        gg = 0;
        hh = 0;
        cc = 0.0;
        inneriter = 0;
        outeriter = 0;
        lastlp = -2000000000000.0;
        lp = lp - sum(0.5 * ((beta2*beta2)*(priorprecbeta))) + sum(0.5 * ((theta2*theta2)*(priorprectheta)));
        err = (abs_err == true) ? fabs(lp - lastlp) : (lp - lastlp);
        while ((err > tolvec(0)) && outeriter < 100) {
          outeriter++;
          
          // UPDATE WORD PARAMETERS
          for (int k = 0; k < K; k++) {
            cc = 1;
            inneriter = 0;
            if (outeriter == 1) stepsize = 0.5;
            while ((cc > tolvec(1)) && inneriter < 10){
              inneriter++;
              lambdak = exp(alpha + psi(k) + beta(k) * theta + beta2(k) * theta2);
              gg = sum(zeta * theta2 * (Y(_,k) - lambdak) / (lambdak + zeta)) - beta2(k) * (priorprecbeta);
              hh = -sum(zeta * (theta2 * theta2) * lambdak * (Y(_,k) + lambdak) / ((lambdak + zeta) * (lambdak + zeta))) - priorprecbeta;
              pars(0,0) = beta2(k);
              newpars(0,0) = pars(0,0) - stepsize * gg / hh;
              beta2(k) = newpars(0,0);
              cc = max(abs(newpars - pars));
              stepsize = 1.0;
            }
          }
          
          
          // UPDATE DOCUMENT PARAMETERS
          for (int i = 0; i < N; i++){
            cc = 1;
            inneriter = 0;
            if (outeriter == 1) stepsize = 0.5;
            while ((cc > tolvec(1)) && inneriter < 10){
              inneriter++;
              lambdai = exp(alpha(i) + psi + beta * theta(i));
              gg = sum(zeta * beta2 * (Y(i,_) - lambdai) / (lambdai + zeta)) - theta2(i) * (priorprectheta);
              hh = -sum(zeta * (beta2 * beta2) * lambdai * (Y(i,_) + lambdai) / ((lambdai + zeta) * (lambdai + zeta))) - priorprectheta;
              pars(0,0) = theta2(i);
              newpars(0,0) = pars(0,0) - stepsize * gg / hh;
              theta2(i) = newpars(0,0);
              cc = max(abs(newpars - pars));
              stepsize = 1.0;
            }
          }
          
          // UPDATE DISPERSION PARAMETERS
          zetatmp = 0.0;
          l_mean = 0.0;
          l2_mean = 0.0;
          for (int k = 0; k < K; k++){
            for (int i=0; i < N; i++){
              mutmp = exp(alpha(i) + psi(k) + beta(k) * theta(i) + beta2(k) * theta2(i));
              l_mean = l_mean + mutmp;
              l2_mean = l2_mean + mutmp * mutmp;
            }
          }
          l_mean = l_mean / (N*K);
          l2_mean = l2_mean / (N*K);
          
          for (int k = 0; k < K; k++){
            for (int i=0; i < N; i++){
              zetatmp = zetatmp + (Y(i,k) - l_mean) * (Y(i,k) - l_mean);
            }
          }
          
          // zeta = zetatmp;
          zeta = l2_mean / ((zetatmp / (N*K-1)) - l_mean);

          theta2 = (theta2 - mean(theta2))/sd(theta2);
          
          // CHECK LOG-POSTERIOR FOR CONVERGENCE
          lastlp = lp;
          lp = -1.0 * (sum(0.5 * ((alpha*alpha) * (priorprecalpha))) + sum(0.5 * ((psi*psi) * (priorprecpsi))) +
            sum(0.5 * ((beta*beta)*(priorprecbeta))) + sum(0.5 * ((theta*theta)*(priorprectheta))) +
            sum(0.5 * ((beta2*beta2)*(priorprecbeta))) + sum(0.5 * ((theta2*theta2)*(priorprectheta))));
          for (int i = 0; i < N; i++){
            for (int k = 0; k < K; k++){
              loglambdaik = alpha(i) + psi(k) + beta(k) * theta(i) + beta2(k) * theta2(i);
              lp = lp + loglambdaik * Y(i,k) - (log(exp(loglambdaik)+zeta)) * (Y(i,k) + zeta);
            }
          }
          // Rprintf("%d: %f2\\n",outeriter,lp);
          //Rcout<<"outeriter="<<outeriter<<"  lp - lastlp= "<<lp - lastlp<<std::endl;
          err = (abs_err == true) ? fabs(lp - lastlp) : (lp - lastlp);
          // END OF WHILE
        }
        // END OF TWO-DIMENSIONAL MODEL
      }
      // END OF NB MODEL
    }
    
    
    // Fix Global Polarity

    // added the -1 because C counts from ZERO...  -- KB
    if (theta(dirvec(0) - 1) > theta(dirvec(1) - 1)) {
        beta = -beta;
        theta = -theta;
    }

    // COMPUTE DOCUMENT STANDARD ERRORS
    for (int i = 0; i < N; i++) {
        lambdai = exp(alpha(i) + psi + beta*theta(i));
        if (method == 1){
          H(0,0) = -sum(lambdai / phi) - priorprecalpha;
          H(1,0) = -sum((beta * lambdai) / phi);
          H(0,1) = H(1,0);
          H(1,1) = -sum(((beta * beta) * lambdai)/phi) - priorprectheta;
        }else if (method == 4){
          H(0,0) = -sum(lambdai / phi) - priorprecalpha;
          H(1,0) = -sum((beta * lambdai) / phi);
          H(0,1) = H(1,0);
          H(1,1) = -sum(((beta * beta) * lambdai)/phi) - priorprectheta;
          hh = -sum((beta2 * beta2) * lambdai / phi) - priorprectheta;
          theta2SE(i) = sqrt(-1.0 / hh);
        }else if (method == 99){
          H(0,0) = -sum(zeta * lambdai * (Y(i,_) + zeta)/((lambdai + zeta) * (lambdai + zeta))) - priorprecalpha;
          H(1,0) = -sum(zeta * lambdai * beta * (Y(i,_) + zeta)/((lambdai + zeta) * (lambdai + zeta)));
          H(0,1) = H(1,0);
          H(1,1) = -sum(zeta * lambdai * beta * beta * (Y(i,_) + zeta)/((lambdai + zeta) * (lambdai + zeta))) - priorprectheta;
        }else if (method == 10000){
          H(0,0) = -sum(zeta * lambdai * (Y(i,_) + zeta)/((lambdai + zeta) * (lambdai + zeta))) - priorprecalpha;
          H(1,0) = -sum(zeta * lambdai * beta * (Y(i,_) + zeta)/((lambdai + zeta) * (lambdai + zeta)));
          H(0,1) = H(1,0);
          H(1,1) = -sum(zeta * lambdai * beta * beta * (Y(i,_) + zeta)/((lambdai + zeta) * (lambdai + zeta))) - priorprectheta;
          hh = -sum(zeta * (beta2 * beta2) * lambdai * (Y(i,_) + lambdai) / ((lambdai + zeta) * (lambdai + zeta))) - priorprectheta;
          theta2SE(i) = sqrt(-1.0 / hh);
        }
        thetaSE(i) = sqrt(-1.0 * H(0,0) / (H(0,0) * H(1,1) - H(1,0) * H(0,1)));
        alphaSE(i) = sqrt(-1.0 * H(1,1) / (H(0,0) * H(1,1) - H(1,0) * H(0,1)));
    }
    
    // COMPUTE WORD STANDARD ERRORS
    for (int k = 0; k < K; k++) {
      lambdak = exp(alpha + psi(k) + beta(k) * theta);
      arma::mat H(2,2);
      if (method == 1){
        H(0,0) = -sum(lambdak / phi[k]) - priorprecpsi;
        H(1,0) = -sum((theta * lambdak) / phi[k]);
        H(0,1) = H(1,0);
        H(1,1) = -sum(((theta * theta) * lambdak) / phi[k]) - priorprecbeta;
      }else if (method == 4){
        H(0,0) = -sum(lambdak / phi[k]) - priorprecpsi;
        H(1,0) = -sum((theta * lambdak) / phi[k]);
        H(0,1) = H(1,0);
        H(1,1) = -sum(((theta * theta) * lambdak) / phi[k]) - priorprecbeta;
        hh = -sum((theta2 * theta2) * lambdak) / phi(k) - priorprecbeta;
        beta2SE(k) = sqrt(-1.0 / hh);
      }else if (method == 99){
        H(0,0) = -sum(zeta * lambdak * (Y(_,k) + zeta)/((lambdak + zeta) * (lambdak + zeta))) - priorprecpsi;
        H(1,0) = -sum(zeta * lambdak * theta * (Y(_,k) + zeta)/((lambdak + zeta) * (lambdak + zeta)));
        H(0,1) = H(1,0);
        H(1,1) = -sum(zeta * lambdak * theta * theta * (Y(_,k) + zeta)/((lambdak + zeta) * (lambdak + zeta))) - priorprectheta;
      }else if (method == 10000){
        H(0,0) = -sum(zeta * lambdak * (Y(_,k) + zeta)/((lambdak + zeta) * (lambdak + zeta))) - priorprecpsi;
        H(1,0) = -sum(zeta * lambdak * theta * (Y(_,k) + zeta)/((lambdak + zeta) * (lambdak + zeta)));
        H(0,1) = H(1,0);
        H(1,1) = -sum(zeta * lambdak * theta * theta * (Y(_,k) + zeta)/((lambdak + zeta) * (lambdak + zeta))) - priorprectheta;
        hh = -sum(zeta * (theta2 * theta2) * lambdak * (Y(_,k) + lambdak) / ((lambdak + zeta) * (lambdak + zeta))) - priorprecbeta;
        beta2SE(k) = sqrt(-1.0 / hh);
      }

      betaSE(k) = sqrt(-1.0 * H(0,0) / (H(0,0) * H(1,1) - H(1,0) * H(0,1)));
      psiSE(k) = sqrt(-1.0 * H(1,1) / (H(0,0) * H(1,1) - H(1,0) * H(0,1)));
    }

    // DEFINE OUTPUT

    return Rcpp::List::create(Rcpp::Named("theta") = theta,
                              Rcpp::Named("alpha") = alpha,
                              Rcpp::Named("psi") = psi,
                              Rcpp::Named("zeta") = zeta,
                              Rcpp::Named("beta") = beta,
                              Rcpp::Named("beta2") = beta2,
                              Rcpp::Named("theta2") = theta2,
                              Rcpp::Named("phi") = phi,
                              Rcpp::Named("thetaSE") = thetaSE,
                              Rcpp::Named("alphaSE") = alphaSE,
                              Rcpp::Named("psiSE") = psiSE,
                              Rcpp::Named("betaSE") = betaSE,
                              Rcpp::Named("theta2SE") = theta2SE,
                              Rcpp::Named("beta2SE") = beta2SE,
                              Rcpp::Named("LogL") = lp);

}
