ARIMA_BIC_GA <- cxxfunction(
  signature(tauClc_ = "numeric", XMat_ = "matrix", Xt_ = "numeric"),
  body = '
    arma::vec tauClc = as<arma::vec>(tauClc_);
    arma::mat XMat = as<arma::mat>(XMat_);
    arma::vec Xt = as<arma::vec>(Xt_);
    int N = Xt.n_elem;

    tauClc[0] = 0.0;
    std::vector<int> tau;
    for (int i = 0; i < tauClc.n_elem; ++i) {
      if (tauClc[i] == 1.0) tau.push_back(i + 1);  // 1-based indexing
    }

    arma::mat DesignX = XMat;
    int m = tau.size();

    if (m > 0) {
      std::vector<int> tmptau = tau;
      tmptau.push_back(N + 1);
      std::sort(tmptau.begin(), tmptau.end());
      tmptau.erase(std::unique(tmptau.begin(), tmptau.end()), tmptau.end());

      arma::mat CpMat = arma::zeros<arma::mat>(N, m);
      for (int i = 0; i < m; ++i) {
        int start = tmptau[i];
        int end = std::min(tmptau[i + 1] - 1, N);
        for (int j = start - 1; j <= end - 1; ++j) {
          CpMat(j, i) = 1.0;
        }
      }
      DesignX = arma::join_rows(XMat, CpMat);
    }

    arma::vec betahat;
    bool success = arma::solve(betahat, DesignX, Xt);
    if (!success) return wrap(1e6);

    arma::vec u = Xt - DesignX * betahat;
    arma::vec u_lag = u.subvec(0, N - 2);
    arma::vec u_now = u.subvec(1, N - 1);

    double denom = arma::dot(u_lag, u_lag);
    if (denom == 0.0) return wrap(1e6);

    double phi_hat = arma::dot(u_now, u_lag) / denom;
    arma::vec eps_hat = u_now - phi_hat * u_lag;
    double sigma2_hat = arma::mean(arma::square(eps_hat));

    double BIC = (N - 1) * std::log(sigma2_hat) + (2 * m + 3) * std::log(N - 1);
    return wrap(-BIC);
  ',
  plugin = "RcppArmadillo"
)