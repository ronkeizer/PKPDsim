runge_kutta4 < state_type > stepr;

struct push_back_solution
{
  std::vector< state_type >& m_states;
  std::vector< double >& m_times;

  push_back_solution ( std::vector< state_type > &states , std::vector< double > &tim )
    : m_states( states ) , m_times( tim ) { }

  void operator()( const state_type &x , double t )
  {
    m_states.push_back( x );
    m_times.push_back( t );
  }
};

struct ode_out {
  vector<double> time;
  vector<state_type> y;
};

ode_out sim_cpp (const NumericVector Ainit, double t_start, double t_end, SEXP par, double step_size) {
  int n_steps = (int) ceil((t_end-t_start)/step_size)+1;
  double t_end_new = t_start + ((n_steps-1) * step_size);
  vector<state_type> x_vec;
  vector<double> tim;
  NumericMatrix A_ret (n_steps, n_comp+2);
  std::fill(A_ret.begin(), A_ret.end(), 0);

  Rcpp::List rparam(par);
  std::string method = Rcpp::as<std::string>(rparam["test"]);

  state_type A = {} ;
  for(int j = 0; j < n_comp; j++) {
    A[j] = Ainit(j);
  }
  ode_out tmp;
  integrate_const (stepr , ode, A, t_start, t_end_new, step_size, push_back_solution (x_vec, tim));
  tmp.y = x_vec;
  tmp.time = tim;
  return(tmp);
}

// [[Rcpp::export]]
List sim_wrapper_cpp (const NumericVector Ainit, NumericVector times, NumericVector doses, int len, SEXP par, double step_size) {
  std::vector<double> t;
  std::vector<state_type> y;
  double t_start, t_end;
  NumericVector Aupd = clone(Ainit);
  for(int i = 0; i < (len-1); i++) {
    t_start = times[i];
    t_end = times[(i+1)];
    Aupd[0] = Aupd[0] + doses[i];
    ode_out tmp = sim_cpp(Aupd, t_start, t_end, par, step_size);
    t.insert(t.end(), tmp.time.begin(), tmp.time.end());
    y.insert(y.end(), tmp.y.begin(), tmp.y.end());
    state_type tail = tmp.y.back();
    for (int k = 0; k < n_comp; k++) {
      Aupd[k] = tail[k];
    }
  }

  List comb;
  comb["time"] = t;
  comb["y"] = y;
  return(comb);
}
