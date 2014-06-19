best full nonlinear SUR:

"silly" classification: http://www.jstor.org/stable/1235997

Good for production function criticism:
The unproductive production function, Journal of agricultural economics [0021-857X] Upton yr:1979 vol:30 iss:2 pg:179

Quite a good overview: http://onlinelibrary.wiley.com/doi/10.1002/jid.4010030102/abstract
Economic efficiency of small farmers in a changing world: A survey of recent evidence

Could be useful for post-estimation:
http://www.sfu.ca/~mrekkas/surv3.pdf

# Loook up using covariates in translog cost function

# todo: think about bootstrapping since CI cant go below zero, and is probably not symmetric

# Idea: check if parameter values are statistically different across the different "technology" groupings. Test them pairwise against the full sample model
# Ok, maybe this is the way to do it: http://www.stata.com/statalist/archive/2012-02/msg00736.html

# TODO: double-gamma parameters (y's times w's) may be not identoified in subsample regression, and we do not deal with it in the cost share equations for the linear case ( or nonlinear case for that matter), so the not-identified double-gammas should be deleted in the cost share equations

# Ok, this article definitely says that theta < 1 means overuse of that input:  Nitrogen efficiency of Dutch dairy farms: a shadow cost system approach

# We almost certainly need to cite this article: "Estimation of Multicrop Production Functions"

# Incorporates output-specific inputs to help pin down efficiency better: http://ideas.repec.org/p/ete/ceswps/ces11.10.html

# Probably also damn useful (by Kuhmbakar) - Also helps defend our price imputation and gives a method for varying alloc. eff. by HH characteristics: Price Distortions and Resource-Use Efficiency in Indian Agriculture: A Restricted Profit Function Approach

# Maybe also useful: Government Interventions, Market Imperfections, and Technical Inefficiency in a Mixed Economy: A Case Study of Indian Agriculture

# put in (one) fixed cost, we need: (logs of all of these) : 
# F itself, F squared (and times every other F), F times every w00, F times every y
# F, F^2, F * w, F * y
# 1 F: 1 + 1 + N + M
# 2 F: 2 + 2^2 + 2*N + 2* M

M <- 5
N <-4



iteration = 128
Step:
  [1] -1.763440e-03 -3.417853e-07  7.194619e-09  1.400157e-09  2.159869e-10
  [6] -1.448380e-06 -2.389704e-06  6.847571e-10 -2.126549e-08  2.709558e-09
 [11]  1.341187e-08  1.281804e-06 -5.817780e-12  2.123276e-03  1.346803e-03
 [16]  6.111331e-03 -7.197711e-13 -1.273953e-12  6.942359e-16 -1.275015e-13
 [21] -1.963593e-09 -1.217610e-12  9.812105e-13 -2.566356e-12  5.084468e-13
 [26]  4.845520e-14  4.407076e-11 -6.515315e-13 -1.439565e-04 -9.616987e-04
 [31]  5.495581e-03 -2.324481e-12 -1.588958e-08 -2.658127e-11 -3.481083e-12
 [36]  4.587255e-12 -3.607865e-12 -1.719067e-13 -1.878490e-12  4.442451e-12
 [41] -9.133878e-12  8.323940e-15  7.438020e-13 -1.097135e-11 -5.372896e-14
 [46]  4.957193e-13 -5.639075e-15  3.994150e-13  2.790345e-14 -2.387608e-11
 [51] -2.252107e-12  2.777482e-14  6.729606e-11  1.827922e-14 -1.466399e-08
 [56]  6.088202e-15  2.247899e-14 -2.980936e-12 -3.304695e-11 -1.170326e-10
 [61] -3.205149e-11 -8.426338e-10 -6.177453e-10 -3.971233e-15 -3.675639e-12
 [66] -9.690951e-15 -7.555571e-11 -2.247726e-11 -1.359401e-12 -2.711961e-14
 [71]  5.862951e-11  9.158661e-12 -4.680184e-13 -2.009810e-12  4.441147e-12
 [76] -9.246931e-12 -2.149015e-13 -6.707360e-12 -1.005285e-11  3.134311e-13
 [81]  9.077004e-04  3.348653e-04 -5.824247e-03 -2.767501e-05 -4.945273e-04
 [86]  7.367632e-04  7.769342e-09 -1.163684e-08  1.845311e-11 -1.516369e-07
 [91] -2.474890e-07 -3.042770e-06  9.073353e-08 -1.180258e-06  4.261064e-06
 [96] -1.113957e-08  2.519711e-06  4.010337e-07  6.949266e-08  3.816069e-10
[101] -6.820513e-12  3.205264e-09  8.271709e-07 -3.190680e-07 -5.216525e-07
[106] -1.059411e-07 -5.571518e-07  2.225470e-08 -2.162944e-09 -1.765778e-06
[111]  3.347036e-08  1.848119e-08 -1.713094e-09 -1.927732e-07 -1.857102e-06
[116]  1.148165e-06  4.438020e-07 -1.331220e-06 -1.152634e-06 -3.978844e-10
[121]  2.994763e-06 -3.571705e-07 -1.802817e-05  8.977742e-05  1.658714e-04
Parameter:
  [1]  4.469983e+00  1.076186e-03 -5.075654e-04  3.619112e-05  3.478840e-04
  [6]  4.670897e-03 -3.311031e-03  1.752689e-04 -8.215406e-04 -1.186377e-03
 [11] -1.241148e-04 -1.595964e-02  1.200346e-05  2.846870e+00 -3.562536e-02
 [16]  5.466482e-01 -2.968923e-08  5.564033e-08 -8.235543e-11 -1.675656e-08
 [21]  5.348099e-06 -4.648421e-08 -1.005552e-07 -2.837883e-07 -3.517930e-07
 [26] -7.297848e-10 -6.723829e-06 -1.891154e-07 -3.399720e-01  3.881011e-02
 [31] -1.954041e+00  1.689312e-07 -1.058099e-04 -1.185900e-06 -1.203211e-07
 [36] -4.911728e-07 -3.041399e-07  1.417162e-07  1.274816e-06  2.765477e-06
 [41]  3.361231e-07  2.046416e-08 -1.351825e-07 -1.017955e-06  1.327898e-07
 [46]  1.682428e-07  1.999622e-07 -9.141333e-07 -2.235019e-08  1.045361e-05
 [51]  3.081577e-07  8.061700e-09 -1.611198e-05  9.713142e-09  8.344487e-05
 [56] -1.171196e-07  2.103200e-08 -1.212030e-06 -4.137141e-06 -5.846120e-06
 [61]  6.116236e-06  1.914387e-04  1.197325e-05  3.433430e-08  4.305759e-07
 [66]  3.517351e-08 -5.925230e-06 -1.806893e-06  8.817747e-07  2.980726e-07
 [71]  3.598412e-05  7.902184e-06 -1.788604e-06 -5.106762e-06  9.075071e-06
 [76] -1.791558e-06 -2.633124e-06  1.679246e-06  3.593665e-06  5.804790e-06
 [81] -1.978172e+00 -8.036762e-02  7.061146e-01  7.159694e-01  1.502436e+00
 [86] -7.365762e-01 -1.243415e-04 -8.519338e-05 -7.559623e-06 -5.954666e-04
 [91] -2.070666e-03 -2.918281e-04 -6.645674e-04  1.153948e-03  6.275687e-03
 [96] -5.711237e-05 -9.543629e-03  2.321366e-03  1.510029e-04  1.091632e-05
[101] -9.215252e-07 -5.065466e-05 -7.804061e-04 -1.527227e-03  1.065607e-03
[106] -4.725250e-04 -6.691631e-04 -8.548661e-05  4.070784e-05 -1.286471e-03
[111]  2.599443e-04 -1.802473e-04  2.921919e-05  4.883858e-04  1.987790e-03
[116] -6.024481e-04 -2.543172e-03 -2.241307e-03 -2.770472e-03  1.462481e-05
[121] -2.633740e-03  5.049472e-04  1.005998e+00  9.560929e-01  1.055057e+00
Function Value
[1] 2804016
Gradient:
  [1]  3.463188e+03  3.581147e+06  1.263487e+06 -1.418763e+06  4.928800e+05
  [6]  2.386724e+05  1.616158e+06 -3.532004e+04  3.904749e+05  6.843708e+04
 [11] -3.532049e+06  7.766661e+03  4.881715e+05  8.124934e+02 -3.507512e+04
 [16]  4.330114e+03  5.918920e+09  5.508733e+09 -2.240458e+11  4.164202e+09
 [21]  2.087797e+08  3.154702e+09 -4.934921e+08  3.756322e+08 -1.781077e+06
 [26] -4.472568e+11 -1.447565e+06  1.919440e+08 -8.583082e+04 -3.782980e+05
 [31] -4.622535e+04  8.761227e+08 -1.658868e+07  2.722855e+07  1.416208e+09
 [36] -6.686181e+07  4.308953e+08  1.375888e+08  9.382860e+06  2.240687e+07
 [41]  6.894390e+08  8.054152e+08  3.665772e+07  5.847389e+07  2.452103e+07
 [46] -8.139871e+07  3.955104e+07  7.403912e+06  6.488315e+07  3.098728e+06
 [51]  3.138042e+08  8.188953e+08 -9.755020e+05  8.640852e+08 -1.658235e+07
 [56] -1.381727e+06  6.910808e+07  1.054775e+07  1.002006e+07  2.053306e+07
 [61]  5.656033e+06  1.756780e+05  2.689959e+07  1.996413e+07  1.180703e+08
 [66]  4.680235e+07  1.906083e+07  4.937972e+07  1.591780e+07  2.061197e+06
 [71] -1.599655e+05  3.076321e+05  9.416816e+06  6.681581e+06  1.125974e+06
 [76]  3.414072e+07  5.417598e+05  2.014242e+07  6.482715e+06  1.700755e+06
 [81] -9.431073e+04 -4.499871e+05 -3.939788e+04 -5.272170e+05 -1.289636e+05
 [86] -4.779726e+05  1.831887e+07  3.520099e+07 -1.010009e+07  2.653820e+07
 [91]  3.882332e+07 -5.705187e+06 -2.311515e+07  3.330501e+07 -5.655596e+06
 [96]  2.894060e+07  3.331256e+07 -7.980104e+06 -1.287394e+08  5.171482e+06
[101] -2.098455e+06 -1.245734e+08  1.093970e+07 -2.795167e+07 -7.044803e+07
[106]  1.767790e+07 -1.651551e+07 -1.252121e+08  2.054254e+07 -8.809319e+06
[111] -1.010398e+08 -2.502931e+07 -4.752948e+07 -1.009212e+08 -2.829818e+07
[116] -5.053045e+07 -9.845046e+07 -2.518565e+07 -4.618209e+07 -9.251780e+07
[121] -2.821617e+07 -4.832406e+07  2.082928e+06 -6.033680e+05  5.586337e+05








It.    2, RSS = 1.0158e+08, Par. =   -11.2465   0.206766 -0.00949779  0.000264324 -0.00856574   0.184142  0.00567597   0.150684   0.356839   0.239383   0.201913  0.0791133 -0.0347391    -2.3538    16.9739    3.59015   -10.9079   0.339644 -3.10587e-08 -1.82429e-10 -5.15555e-08 -4.91528e-06 -1.38804e-07 -1.49601e-07 -7.83236e-07 -8.28525e-07 -1.14802e-06 -2.42801e-05 -2.44617e-06  -0.426558   -2.00446 -0.0747582   -1.13403 -4.40035e-08  8.51069e-07 -4.14373e-06  7.17207e-07 -2.38395e-09  1.2936e-07  1.4387e-07 -5.73749e-07 -8.43622e-08 -1.89406e-06  1.34777e-07 -1.01458e-07 -7.16246e-07 -1.74278e-06 -5.01573e-07 -5.31208e-07 -3.93798e-07 -3.90871e-07 -1.57209e-06  4.62951e-06 -2.03206e-07 -4.32856e-08 -1.07236e-05 -6.70691e-08 -3.15883e-05  3.43942e-06 -1.46693e-06 -6.37864e-06  8.06263e-07  8.21612e-07 -5.71205e-07  0.00466799 -1.54496e-06  3.98153e-07  1.42793e-06 -8.98082e-07  3.78135e-06 -1.13162e-06 -3.80058e-08  5.96916e-06  5.6066e-05  3.35391e-07 -3.34396e-07  6.40312e-07 -2.01856e-08 -4.66876e-07 -1.38411e-06 -4.64311e-06 -3.88537e-06  3.06353e-06    -0.6362  0.0127599    2.28611  -0.752825    1.78704  -0.369704  0.000773739 -0.000220537 -3.32367e-05  0.000544209 -0.00289719 -0.000456464 -0.00107149 -5.60373e-05 -0.00322991 -0.000391968  0.0545491 -0.00931416  0.00058122 -0.000240365 -3.7295e-05 -0.000709217   0.161758   0.010854  0.00212161  0.00279683 -0.00193178 -0.000626399   0.100973  7.67955e-05  0.000496921 -0.000300986 -1.18819e-05 -0.000353566 -0.0677057  -0.023079  0.0015778 -0.000204165 -0.00800896 -0.000634281   0.222561 -0.00457965  0.0299072 -6.00171e-05  1.01558e-05  0.00118608   0.220137 -0.00137586 -0.00121356 -0.000516004 -0.0228574 -0.000638423   0.166803  0.00978752   0.901329   0.720617    1.89033    1.06225


iteration = 1000
Parameter:
 -8.079502e-01  9.191934e-04 -5.671731e-04  4.680930e-05 -5.333195e-04
  3.633371e-03 -4.096313e-03  1.102803e-03 -5.300592e-03 -4.085694e-03
 -9.520525e-04 -6.468393e-03  6.212633e-04  2.507418e+00 -5.003781e-01
 -7.145091e+00 -9.629116e-08  2.702008e-09  5.333564e-11  6.372790e-08
  6.047963e-06 -4.984668e-08 -3.073548e-07 -8.908528e-07 -1.497947e-06
  1.668652e-10  7.055257e-06  8.814609e-07  2.323244e+00  5.573547e-01
 -3.969458e+00  1.433798e-07 -1.121403e-04 -2.180878e-06 -1.148929e-07
 -2.679434e-08  2.738102e-07  8.237854e-07  1.184477e-06  9.582295e-07
  3.995806e-07  6.204852e-09  1.118353e-07  1.807992e-07  8.278157e-08
 -2.747216e-07  1.446973e-06 -5.969027e-07  2.134001e-08  1.207934e-05
 -1.009794e-07 -7.585802e-10 -1.808592e-05  1.621311e-08  1.112020e-04
  1.472487e-06  2.742190e-08 -3.936958e-06 -2.127557e-06 -2.374857e-06
  1.026141e-05  1.508406e-04  9.813443e-06 -1.379024e-07  4.821556e-07
  9.718360e-07 -5.413186e-06 -2.291635e-06 -2.679836e-06 -1.499661e-05
  4.369957e-05 -1.178264e-05  9.211955e-07 -1.771845e-06  9.382947e-06
 -2.067269e-06 -1.917993e-05 -1.760923e-06  2.911599e-06  1.693091e-05
 -2.831685e+00  4.073757e-01  5.150573e+00  1.824563e-01  9.905260e-02
 -9.719284e-01  1.043999e-04  4.855842e-04  3.040150e-06  7.932830e-04
  3.065502e-05  7.133997e-04  1.399226e-03  2.754440e-04  5.430944e-03
  9.643155e-04 -2.084830e-02  4.164190e-04 -1.174454e-04 -1.516253e-04
  3.185920e-05  1.450887e-04 -1.056485e-03 -1.883782e-03  2.663229e-04
 -6.072183e-04 -1.899100e-03 -1.116963e-05  4.161910e-03 -1.528253e-04
  7.760457e-04 -5.291840e-04 -1.840694e-05 -1.018726e-03  7.301908e-03
 -4.037017e-04 -1.726120e-03 -3.989594e-03 -3.089123e-03 -1.502506e-03
  9.335562e-05  1.387015e-03  1.007655e+00  9.990633e-01  9.654531e-01
  
Function Value
[1] 3880.254


single eqn:

iteration = 563
Step:
  [1]  4.122203e-08  2.852824e-10 -4.098350e-10  6.669997e-12  9.557056e-11
  [6] -8.561834e-10  1.927099e-09 -2.506248e-10  2.299749e-09 -8.280123e-11
 [11]  1.058959e-09  1.968075e-09 -1.329947e-13  5.191544e-08 -1.022384e-07
 [16] -2.248818e-06  9.455620e-14 -1.182624e-16  9.254489e-19 -4.306358e-14
 [21] -9.326050e-13 -6.036672e-16  2.433893e-13  1.457550e-13 -5.587034e-13
 [26] -8.547020e-19  3.336315e-13 -7.167751e-14 -2.623140e-07 -8.290969e-07
 [31]  5.354036e-06 -3.111421e-14  4.679054e-12  4.748059e-13  2.516977e-14
 [36]  4.790043e-16  8.320489e-14 -7.242104e-13 -9.551742e-14 -3.821328e-14
 [41] -6.363929e-14 -2.053014e-16  2.404108e-14 -6.809300e-15 -5.319990e-17
 [46]  8.084590e-14 -5.445925e-14  2.201091e-14  5.596050e-16 -2.084437e-12
 [51] -1.198993e-15 -4.991552e-18  3.526814e-12 -2.808284e-15 -7.021535e-12
 [56]  7.922889e-14  5.939076e-16 -2.216040e-13  1.815413e-13  2.496778e-13
 [61]  2.610606e-13 -8.363388e-12 -2.228773e-12  1.600555e-15 -1.045163e-14
 [66] -9.921642e-14  1.345228e-12  3.737724e-13  5.738219e-13 -1.734702e-12
 [71]  3.006275e-12  4.293945e-13  5.526466e-14  5.810239e-14 -4.426684e-13
 [76]  4.459769e-13  2.950270e-13 -5.632743e-14 -5.011296e-13 -5.842547e-13
 [81]  8.896706e-07 -9.889794e-09 -4.078610e-07  4.261265e-09  2.910348e-09
 [86]  7.513661e-07 -1.670763e-11 -3.786834e-10  3.216368e-15  2.157333e-10
 [91] -1.163930e-13  2.689890e-10 -9.958735e-10  2.122309e-12 -4.035837e-09
 [96] -2.388218e-10  4.067528e-09 -7.971033e-11  2.130439e-11 -3.190194e-11
[101] -1.552372e-11 -1.523737e-11  4.546737e-10  5.956512e-10  3.429300e-11
[106]  8.005439e-11  1.017836e-09  1.817384e-12 -1.464215e-09  7.961481e-12
[111] -1.045229e-09  5.871236e-10 -1.240836e-12  5.139533e-10 -2.231497e-09
[116]  7.163906e-11 -4.425954e-10  2.115871e-10 -1.087953e-09 -1.468697e-09
[121] -3.130439e-13 -6.447106e-10 -2.388360e-09 -9.617506e-09 -1.706874e-07
Parameter:
  [1] -8.057730e-01  9.332670e-04 -5.845969e-04  4.710933e-05 -5.294802e-04
  [6]  3.598076e-03 -4.023962e-03  1.093003e-03 -5.199475e-03 -4.088891e-03
 [11] -9.053676e-04 -6.386084e-03  6.213079e-04  2.508311e+00 -5.058584e-01
 [16] -7.277740e+00 -9.229366e-08  2.697036e-09  5.337739e-11  6.191914e-08
 [21]  6.009600e-06 -4.987681e-08 -2.970863e-07 -8.846175e-07 -1.521214e-06
 [26]  1.668373e-10  7.069219e-06  8.785228e-07  2.309311e+00  5.197759e-01
 [31] -3.772248e+00  1.420984e-07 -1.119423e-04 -2.161064e-06 -1.138487e-07
 [36] -2.677404e-08  2.773050e-07  7.937361e-07  1.180525e-06  9.566658e-07
 [41]  3.970017e-07  6.196351e-09  1.128320e-07  1.805167e-07  8.277937e-08
 [46] -2.713398e-07  1.444702e-06 -5.959885e-07  2.136340e-08  1.199271e-05
 [51] -1.010289e-07 -7.587838e-10 -1.793939e-05  1.609760e-08  1.109146e-04
 [56]  1.475772e-06  2.744676e-08 -3.946133e-06 -2.120009e-06 -2.364471e-06
 [61]  1.027234e-05  1.504942e-04  9.721291e-06 -1.378359e-07  4.817094e-07
 [66]  9.676931e-07 -5.357305e-06 -2.276035e-06 -2.655925e-06 -1.506853e-05
 [71]  4.382452e-05 -1.176462e-05  9.235292e-07 -1.769420e-06  9.364657e-06
 [76] -2.048682e-06 -1.916758e-05 -1.763247e-06  2.890818e-06  1.690692e-05
 [81] -2.799820e+00  4.037089e-01  5.078606e+00  1.819552e-01  9.915036e-02
 [86] -9.577286e-01  1.036896e-04  4.699558e-04  3.040264e-06  8.025189e-04
 [91]  3.065018e-05  7.248762e-04  1.357123e-03  2.755206e-04  5.263153e-03
 [96]  9.492520e-04 -2.068442e-02  4.131000e-04 -1.166514e-04 -1.528758e-04
[101]  3.118678e-05  1.444874e-04 -1.037672e-03 -1.855812e-03  2.676518e-04
[106] -6.040818e-04 -1.856952e-03 -1.117817e-05  4.100505e-03 -1.525031e-04
[111]  7.309126e-04 -5.046030e-04 -1.846305e-05 -9.969199e-04  7.208072e-03
[116] -4.005985e-04 -1.746379e-03 -3.985035e-03 -3.134477e-03 -1.563060e-03
[121]  9.334247e-05  1.359978e-03  1.005322e+00  1.000027e+00  9.529390e-01
Function Value
[1] 3820.427
Gradient:
  [1] -1.073729e+01  1.978319e+05 -4.574953e+04  6.994260e+05 -8.936358e+04
  [6]  7.010878e+03 -3.074215e+04  6.682850e+04  9.329807e+03 -5.216997e+00
 [11] -5.945804e+04 -3.583214e+03  7.808932e+03  1.607080e+04  1.665771e+05
 [16]  1.613993e+04 -2.202352e+08  6.108738e+08  8.622006e+10  4.520319e+08
 [21]  2.949958e+06 -6.305875e+07 -7.433432e+07  3.996505e+06  2.009851e+07
 [26]  7.354989e+08 -4.760967e+05  1.213260e+07  8.739962e+04  4.379363e+05
 [31]  7.604317e+04  1.829537e+08 -8.036907e+04 -8.195137e+06 -1.672048e+08
 [36] -6.854317e+06 -5.403007e+07  1.024256e+08  7.436121e+06  6.052654e+06
 [41]  7.325230e+07  5.448890e+08 -2.003041e+08  1.995348e+07  7.653546e+05
 [46] -6.916178e+07  1.428361e+06 -6.057292e+06 -8.865396e+07  1.271788e+06
 [51]  1.045851e+07  1.434397e+09 -1.007838e+06  1.387392e+09  1.301614e+04
 [56] -3.690944e+06 -5.392011e+07  1.499767e+06 -3.665919e+06 -3.981983e+06
 [61] -1.848272e+05  3.781754e+04  2.462377e+06 -7.901221e+06  2.299213e+06
 [66]  9.494786e+06 -4.253753e+06 -5.374410e+06 -6.556071e+06  7.706635e+05
 [71] -1.464423e+05 -1.710104e+05 -1.687133e+06 -1.479366e+06  5.529799e+05
 [76] -8.557417e+06 -5.621057e+04  2.048685e+06  5.888874e+06  2.619649e+05
 [81]  8.216787e+04  8.523688e+05  8.254749e+04  9.488133e+05  1.634943e+05
 [86]  8.279608e+05  1.290261e+04  2.129278e+05 -2.078615e+05  1.833717e+04
 [91]  1.150934e+04 -2.025582e+04  1.118349e+04 -1.599005e+04  1.351164e+04
 [96]  3.985832e+09 -1.680946e+03  4.041201e+04 -7.219093e+05  5.540982e+05
[101] -1.135892e+06  2.480293e+05 -4.198726e+04  3.629329e+04 -1.735480e+05
[106] -6.523614e+04 -2.551653e+04  4.137924e+10  5.751154e+03 -5.625704e+04
[111] -8.522508e+04 -8.862943e+04 -9.208117e+05  1.033124e+04  3.029341e+03
[116] -3.997115e+03 -4.624693e+04 -2.413163e+04  1.102030e+04  4.005949e+09
[121]  2.305052e+03  2.365576e+04 -4.707727e+08 -7.234258e+08  1.140856e+07






 -1.164353e+00  2.306744e-04  5.996563e-03  3.783412e-03  1.295889e-04
  4.594726e-03 -2.746313e+00 -1.225206e-01  1.374865e-02 -1.790100e-08
 -1.547928e-07 -1.570154e-06  3.775979e-08  1.601061e-05  1.936514e+00
  1.364754e+00  1.728536e+00  9.018337e-07  4.922581e-08  4.269121e-07
 -3.297513e-07 -1.454488e-07  1.829481e-05  1.660571e-06  3.610227e-05
  2.434729e-06 -7.589386e-01 -1.979269e+00  4.046668e+00 -4.010761e-01
  1.789087e+00 -1.844894e+00  3.672204e-06  2.809829e-05 -4.081711e-04
  5.554004e-04  5.966679e-04 -7.393837e-06  7.486729e-06 -2.733781e-03
 -7.137061e-05 -2.668370e-03 -2.860235e-05 -2.130847e-03  5.416662e-04
  2.658160e-04 -7.566107e-04  1.680922e-02  1.415979e-02  6.561629e+00



try.params <- readLines(pipe("pbpaste"))

try.params <- unlist(strsplit(paste0(try.params, collapse=" "), "( +)|(\n+)"))
try.params <- try.params[try.params!=""]
try.params <- as.numeric(try.params)

names(try.params) <- names(args.list[[1]])
# OR:
names(try.params) <- names(ln.E.start.vals)


fm1DNase2 <- nlsLM(nls.formula.ln.E, data=as.data.frame(args.list[-1]),
start=try.params, trace=TRUE, control=list(factor=.01, maxiter=16, maxfev=2147483647))

summary(fm1DNase2 )

# Our flow must be:
# 1. Construct nonlinear string
# 2. Impose symmetry restrictions on this string
# 3. Contruct linear string from this nonlinear string
# 4. Make the system of equations
# 5. Create crossreferene dataframe
# 6. Determine which vars are singular, and note them
# 7. Delete them from all the equations

# 46536.06


# put in (one) fixed cost, we need: (logs of all of these) : 
# F itself, F squared (and times every other F), F times every w00, F times every y
# F, F^2, F * w, F * y
# 1 F: 1 + 1 + N + M
# 2 F: 2 + 2^2 + 2*N + 2* M
# zeta.single, zeta.double, kappa, delta








# To retroactively remove fixed costs, we can just set q01=0 for all elements of q01


library(systemfit)

# ln.E.data <- log(w01*x01 + w02*x02 + w03*x03 + w04*x04 + w05*x05 )
 ln.E.data <- log(w01*x01 + w02*x02 + w03*x03 + w04*x04 + 1  )
 
# ln.E.data <- log(w01*x01 + w02*x02 + w03*x03 + w04*x04 + w05*x05 + 1 )

# M <- 5
# M <- 12
M <- 1
N <-5
J <- 2

lead.zero <- function(x) {formatC(x, width = 2, flag = "0")}

# llf.creator.fn <- function(M, N, Z) 

y.perm <- expand.grid(paste0("y", lead.zero(1:M)), paste0("y", lead.zero(1:M)))
M.2.dim <- gsub( "y", ".", do.call(paste0, y.perm))

N.2.dim <- expand.grid(paste0(".", lead.zero(1:N)), paste0(".", lead.zero(1:N)))

J.2.dim <- expand.grid(paste0(".", lead.zero(1:J)), paste0(".", lead.zero(1:J)))

ln.sh.w.grid <- expand.grid( paste0("log(w", lead.zero(1:N), " * theta", lead.zero(1:N), ") * "),
  paste0("log(w", lead.zero(1:N), " * theta", lead.zero(1:N), ")") )
  
ln.sh.q.grid <- expand.grid( paste0("log(q", lead.zero(1:J),  ") * "),
  paste0("log(q", lead.zero(1:J),  ")") )

ln.sh.w.grid.2 <- do.call(paste0, ln.sh.w.grid)

ln.sh.q.grid.2 <- do.call(paste0, ln.sh.q.grid)

M.N.dim <- expand.grid(paste0(".", lead.zero(1:M)), paste0(".", lead.zero(1:N)))

ym.wn <- expand.grid(paste0("y", lead.zero(1:M)), paste0("log(w", lead.zero(1:N), " * theta", lead.zero(1:N), ")"))

M.J.dim <- expand.grid(paste0(".", lead.zero(1:M)), paste0(".", lead.zero(1:J)))

ym.qj <- expand.grid(paste0("y", lead.zero(1:M)), paste0("log(q", lead.zero(1:J), ")"))

J.N.dim <- expand.grid(paste0(".", lead.zero(1:J)), paste0(".", lead.zero(1:N)))

qj.wn <- expand.grid(paste0("log(q", lead.zero(1:J), ")"), paste0("log(w", lead.zero(1:N), " * theta", lead.zero(1:N), ")"))

# data.frame(M.N.dim, ym.wn) # checks out  data.frame(M.J.dim, ym.qj)

# ln.c part
# beta0 + 
ln.c.1 <- paste0("alpha", lead.zero(1:M), " * " , 
 "y", lead.zero(1:M), collapse=" + ")
# +
ln.c.2 <-  paste0("beta", lead.zero(1:N),  " * ",
 "log(w", lead.zero(1:N), " * ", "theta", lead.zero(1:N), ")", collapse=" + ")
# + (1/2) *
ln.c.3 <-  paste0("alpha", M.2.dim , " * ", y.perm[[1]], " * ", y.perm[[2]], collapse=" + ")
# + (1/2) *
ln.c.4 <-  paste0("beta", do.call(paste0, N.2.dim ), " * ", ln.sh.w.grid.2, collapse=" + ")
# +
ln.c.5 <- paste0("gamma", do.call(paste0, M.N.dim), " * ", ym.wn[[1]], " * ", ym.wn[[2]], collapse=" + " )

ln.c.6 <-  paste0("zeta", lead.zero(1:J),  " * ", "log(q", lead.zero(1:J), ")", collapse=" + ")
 
ln.c.7 <-  paste0("zeta", do.call(paste0, J.2.dim ), " * ", ln.sh.q.grid.2, collapse=" + ")

ln.c.8 <-  paste0("kappa", do.call(paste0, J.N.dim), " * ", qj.wn[[1]], " * ", qj.wn[[2]], collapse=" + " )

ln.c.9 <-  paste0("delta", do.call(paste0, M.J.dim), " * ", ym.qj[[1]], " * ", ym.qj[[2]], collapse=" + " )

ln.c <- paste0("beta0 + ", ln.c.1, " + ", ln.c.2, " + (1/2) * (", ln.c.3, 
  ") + (1/2) * (", ln.c.4, ") + ", ln.c.5, " + ", ln.c.6, " + (1/2) * (", ln.c.7 ,
  ") + ", ln.c.8, " + ", ln.c.9)



gamma.special<-c()
gamma.mat<-matrix(1:(M*N), nrow=M, ncol=N)
for ( i in 1:N) {
  gamma.special[i] <- paste0(
    paste0("gamma", do.call(paste0, M.N.dim), " * ", ym.wn[[1]])[gamma.mat[, i]],
    collapse=" + " )
}

beta.special<-c()
beta.mat<-matrix(1:(N*N), nrow=N, ncol=N)
for ( i in 1:N) {
  beta.special[i] <- paste0(
    paste0("beta", do.call(paste0, N.2.dim[2:1]) , " * ", gsub("[*] $", "", ln.sh.w.grid[[1]]))[beta.mat[, i]],
    collapse=" + " )
}

kappa.special<-c()
kappa.mat<-matrix(1:(J*N), nrow=J, ncol=N)
for ( i in 1:N) {
  kappa.special[i] <- paste0(
    paste0("kappa", do.call(paste0, J.N.dim), " * ", qj.wn[[1]])[kappa.mat[, i]],
    collapse=" + " )
}


ln.E.2nd <- paste0( "log(" , 
  paste0(
  paste0("(w", lead.zero(1:N), " / (w", lead.zero(1:N), " * theta", 
   lead.zero(1:N), ")) * ", 
   "(beta", lead.zero(1:N), " + ",
   gamma.special, " + ",
   beta.special, " + ",
   kappa.special, ")"),
  collapse=" + " ), ")" )
   
# ln.E <- paste0("test.fn <- function(X) {", ln.c, " + ", ln.E.2nd, "}")





#test.nls <- nls(nls.formula.ln.E, trace=TRUE)

#ln.E.string <- paste(ln.c, " + ", ln.E.2nd)

ln.E.string <- ln.c


#### imposing symmetry restrictions

library(stringr)


ln.E.vars <- all.vars(as.formula(paste("ln.E.data ~", ln.E.string)))
ln.E.vars <- ln.E.vars[ !grepl("(w[0-9])|(q[0-9])|(y[0-9])|(ln.E.data)", ln.E.vars ) ]
ln.E.vars <- sort(ln.E.vars)


# replacements <- data.frame(greek=c("alpha", "beta", "gamma"), N=c(M,N,N), M=c(M,N,M))
# so do not actually need for gamma
replacements <- data.frame(greek=c("alpha", "beta", "zeta"), N=c(M,N,J), M=c(M,N,J))
if (J==1) {replacements <- replacements[1:2, ] }


for ( k in 1:nrow(replacements) ) {

if (replacements$greek[k]=="alpha" & M==1) {next}

symm <- ln.E.vars[grepl(paste0(replacements$greek[k], "[.]"), ln.E.vars) ]

symm.mat<-matrix(paste0(replacements$greek[k], ".", apply(X=expand.grid(lead.zero(1:max(N,M)), lead.zero(1:max(N,M))), MARGIN=1, FUN=paste, collapse=".")), nrow=max(N,M), ncol=max(N,M))
symm.mat[upper.tri(symm.mat, diag = FALSE)] <- t(symm.mat)[upper.tri(symm.mat, diag = FALSE)]
symm.mat<-symm.mat[1:replacements$N[k], 1:replacements$M[k]]

# data.frame(symm, c(symm.mat))

for ( i in 1:length(symm)) {
  ln.E.string <- str_replace_all(ln.E.string, symm[i], c(symm.mat)[i])
}

}

all.vars(as.formula(paste("ln.E.data ~", ln.E.string)))

#######################

# TODO: double check the symmetry replacements

ln.E <- paste0("nls.formula.ln.E <- ln.E.data ~ ", ln.E.string)

#function.text <- llf.creator.fn(12,5,1)
eval(parse(text=ln.E))




# load(file=paste0(work.dir, "firm df.Rdata"))
#### Ok, what would an lm-compatible formula look like?
#test.df <- data.frame(x=runif(500), y=runif(500))
#summary(lm(y ~ x, data=test.df ))

# Let's try direct modification of 
#beta01 * log(w01 * theta01) 
#alpha01 *
#alpha.01.01 * y01 * y01
#beta.01.01 * log(w01 * theta01) * log(w01 * theta01)
#gamma.01.03 * y01 * log(w03 * theta03)

#restrict.matrix = "Investment_(Intercept) = Consumption_(Intercept)"

#We can only use the first part, which means the parameter restrictions will go away:
ln.c <- ln.E.string 
ln.c.linear <- ln.c

ln.c.linear <- gsub("beta0 [+]", "", ln.c.linear)
ln.c.linear <- gsub("beta[0-9]+ [*] ", "", ln.c.linear)
ln.c.linear <- gsub(" [*] theta[0-9]+", "", ln.c.linear)
ln.c.linear <- gsub("alpha[0-9]+ [*]", "", ln.c.linear)
ln.c.linear <- gsub("alpha.[0-9]+.[0-9]+ [*] ", "", ln.c.linear)
ln.c.linear <- gsub("beta.[0-9]+.[0-9]+ [*]", "", ln.c.linear)
ln.c.linear <- gsub("gamma.[0-9]+.[0-9]+ [*]", "", ln.c.linear)
ln.c.linear <- gsub("zeta[0-9]+ [*] ", "", ln.c.linear)
ln.c.linear <- gsub("zeta.[0-9]+.[0-9]+ [*]", "", ln.c.linear)
ln.c.linear <- gsub("kappa.[0-9]+.[0-9]+ [*]", "", ln.c.linear)
ln.c.linear <- gsub("delta.[0-9]+.[0-9]+ [*]", "", ln.c.linear)

ln.c.linear <- gsub("[(]1/2[)] [*]", "", ln.c.linear)
# we should just scale the parameters to get the 1/2 part down
ln.c.linear <- gsub("[*]", ":", ln.c.linear)
# convert to single interaction term
for (i in 1:max(c(N,M))) {
  temp.y <- paste0("y", lead.zero(i))
  ln.c.linear <- gsub(paste0(temp.y, " : ", temp.y), paste0("I(",temp.y, "^2)"), ln.c.linear)
  temp.w <- paste0("w", lead.zero(i))
  ln.c.linear <- gsub(paste0("log[(]", temp.w, "[)] : log[(]", temp.w, "[)]"), paste0("I(log(", temp.w, ")^2)"), ln.c.linear)
  temp.q <- paste0("q", lead.zero(i))
  ln.c.linear <- gsub(paste0("log[(]", temp.q, "[)] : log[(]", temp.q, "[)]"), paste0("I(log(", temp.q, ")^2)"), ln.c.linear)
}

#test.lm <-lm(as.formula(paste0("ln.E.data ~", ln.c.linear)))

# So we can impose linear restrictions after all


S.n.H <- list()

for ( n in 1:N) {
 S.n.H[[n]] <- as.formula(
   paste0( "I( (x", lead.zero(n), " * ", "w", lead.zero(n), ")/exp(ln.E.data)) ~ ",
    paste0("log(w", lead.zero(1:N), ")", collapse=" + "), " + ", 
    paste0("y", lead.zero(1:M), collapse=" + "), " + ", 
    paste0("log(q", lead.zero(1:J), ")", collapse=" + ")
   )
  )
  names(S.n.H)[n] <- paste0("S.n.H.", lead.zero(n))
}
S.n.H[[1]] <- NULL


S.n.H[[length(S.n.H)+1]] <- as.formula(paste0("ln.E.data ~", ln.c.linear))
names(S.n.H)[length(S.n.H)] <- "cost.fn"


test.lm <-lm(terms(S.n.H[[length(S.n.H)]], keep.order=TRUE))

length(attr(terms(S.n.H[[length(S.n.H)]], keep.order=TRUE),"term.labels"))


#names(coef( test.lm))
#names(coef( test.lm)[is.na(coef( test.lm))])
#kleinOls <- systemfit( S.n.H[1:4], "SUR", maxit = 1  )
#names(coef( kleinOls))

# We need a cross reference for the parameter names

# So we have it with:
# A. Linear fit (cross equation parameter restrictions)
#   1. Cost function
#   2. Cost share
# B. Nonlinear fit (Just replace appropriate parameters)
#   1. Cost function
#   2. Cost share



ln.E.vars <- all.vars(as.formula(paste("ln.E.data ~", ln.c)))
# ln.E.vars <- all.vars(as.formula(paste("ln.E.data ~", ln.E.string)))
ln.E.vars <- ln.E.vars[ !grepl("(theta[0-9])|(w[0-9])|(q[0-9])|(y[0-9])|(ln.E.data)", ln.E.vars ) ]
ln.E.vars
param.crossref.df <- data.frame(nlm=ln.E.vars, lm=names(coef(test.lm)), lm.share=NA, stringsAsFactors=FALSE)
# already has symmetry

for ( i in grep("kappa", param.crossref.df$nlm)) {
  param.crossref.df$lm[i] <- paste0(gsub(".+:", "", param.crossref.df$lm[i]), ":", gsub(":.+", "", param.crossref.df$lm[i]) )
}



for ( i in lead.zero(2:N)) {
  param.crossref.df[param.crossref.df$nlm==paste0("beta", i), "lm.share"] <- 
    paste0("S.n.H.", i, "_(Intercept)")
  for ( j in lead.zero(1:M)) {
    param.crossref.df[
      param.crossref.df$nlm==paste0("beta.", i, ".", j), "lm.share"] <-
    paste0("S.n.H.", i, "_", "log(w", j, ")")
    param.crossref.df[
      param.crossref.df$nlm==paste0("gamma.", j, ".", i), "lm.share"] <-
    paste0("S.n.H.", i, "_", "y", j)
  }
  for ( j in lead.zero(1:J)) {
      param.crossref.df[
      param.crossref.df$nlm==paste0("kappa.", j, ".", i), "lm.share"] <-
    paste0("S.n.H.", i, "_", "log(q", j, ")")
  }
}

#singular.test.lm <-lm(terms(S.n.H[[length(S.n.H)]], keep.order=TRUE))
orig.singular.model <- S.n.H[[length(S.n.H)]]
singular.test.lm <-lm(orig.singular.model)
singular.terms <- names(coef(singular.test.lm )[is.na(coef(singular.test.lm ))])
# singular.terms <- gsub(":", " : ", singular.terms)
#singular.terms <- paste0(" [+] ", singular.terms)

S.n.H.cost.fn.string <- as.character(S.n.H[[length(S.n.H)]])[3]
S.n.H.cost.fn.string <- gsub("\n", "", S.n.H.cost.fn.string )
S.n.H.cost.fn.string <- gsub(" +", " ", S.n.H.cost.fn.string )


# linear.terms.for.adding.up <-lm(terms(S.n.H[[length(S.n.H)]], keep.order=TRUE))

linear.terms.for.adding.up <-lm(terms(as.formula(paste0("ln.E.data ~ ", S.n.H.cost.fn.string)), keep.order=TRUE))

linear.terms.for.adding.up <- paste0(names(coef(linear.terms.for.adding.up)), collapse=" + ")

S.n.H.cost.fn.string <- gsub("[(]Intercept[)] .", "",  linear.terms.for.adding.up)


for ( i in 1:M) {
#  S.n.H.cost.fn.string <- gsub(paste0(":y", lead.zero(i)),  
#    paste0(":I(0.5*y", lead.zero(i), ")"), S.n.H.cost.fn.string)
# So the cross terms are supposed to appear twice, but don't, so we can choose not to divide by 0.5 and get the same effect. The squared terms are supposed to appear only once.
  S.n.H.cost.fn.string <- gsub(paste0("I[(]y", lead.zero(i), ".2[)]"),   
    paste0("I(0.5*y", lead.zero(i), "^2)"), S.n.H.cost.fn.string)  
    
}



for ( i in 1:N) {
#  S.n.H.cost.fn.string <- gsub(paste0("log[(]w", lead.zero(i), "[)]:"),  
#    paste0("I(0.5*log(w", lead.zero(i), ")):"), S.n.H.cost.fn.string)
  S.n.H.cost.fn.string <- gsub(paste0("I[(]log[(]w", lead.zero(i), "[)].2[)]"),   
    paste0("I(0.5*log(w", lead.zero(i), ")^2)"), S.n.H.cost.fn.string) 
}

for ( i in 1:J) {
#  S.n.H.cost.fn.string <- gsub(paste0("log[(]w", lead.zero(i), "[)]:"),  
#    paste0("I(0.5*log(w", lead.zero(i), ")):"), S.n.H.cost.fn.string)
  S.n.H.cost.fn.string <- gsub(paste0("I[(]log[(]q", lead.zero(i), "[)].2[)]"),   
    paste0("I(0.5*log(q", lead.zero(i), ")^2)"), S.n.H.cost.fn.string) 
}

linear.terms.for.adding.up <- S.n.H.cost.fn.string


#length(attr(terms(as.formula(paste0("ln.E.data ~ ", S.n.H.cost.fn.string)), keep.order=TRUE), "term.labels"))

#names(coef(linear.terms.for.adding.up))

# there are a bunch of duplicates or something

orig.singular.model <- as.formula(paste0("ln.E.data ~ ", S.n.H.cost.fn.string))
singular.test.lm <-lm(orig.singular.model)
singular.terms <- names(coef(singular.test.lm )[is.na(coef(singular.test.lm ))])


# gsub("(9)", "", "6v ghu676b(9)", fixed=TRUE)

for ( i in singular.terms) {
  S.n.H.cost.fn.string <- gsub(paste0(" + ", gsub(" ", "", i)), "", S.n.H.cost.fn.string, fixed=TRUE)
  #rev.temp<-paste0(gsub("y[0-9]+:", "", i), ":", gsub(":y[0-9]+", "", i))
  # I don't think I need the reversal anymore
  # S.n.H.cost.fn.string <- gsub(paste0(" [+] ", rev.temp), "", S.n.H.cost.fn.string, fixed=TRUE)
}


# S.n.H.cost.fn.string <-gsub(" : ", ":", S.n.H.cost.fn.string)



S.n.H[[length(S.n.H)]] <- as.formula(paste0("ln.E.data ~ ", S.n.H.cost.fn.string))

kleinOls <- systemfit( S.n.H, "SUR", maxit = 1  )


singular.test.lm <-lm(terms(S.n.H[[length(S.n.H)]], keep.order=TRUE))

singular.test.lm.names <- names(coef(singular.test.lm ))

for ( i in grep("log[(]q[0-9][0-9][)]:log[(]w[0-9][0-9][)]", param.crossref.df$lm) ) {
  param.crossref.df$lm[i] <- paste0(gsub(".+:", "", param.crossref.df$lm[i]), ":", gsub(":.+", "", param.crossref.df$lm[i]) )
}

param.crossref.no.singular.df <- 
  param.crossref.df[param.crossref.df$lm %in% singular.test.lm.names, ]
  
lm.param.restrictions <- paste0("cost.fn_", param.crossref.no.singular.df$lm[!is.na(param.crossref.no.singular.df$lm.share)], " = ", 
  param.crossref.no.singular.df$lm.share[!is.na(param.crossref.no.singular.df$lm.share)]
)

lm.param.restrictions <- c(lm.param.restrictions, 
  paste0("cost.fn_I(0.5 * log(w", lead.zero(2:N), ")^2) = S.n.H.", lead.zero(2:N), "_log(w", lead.zero(2:N),")")
)

#if (J>1) {
#lm.param.restrictions <- c(lm.param.restrictions, 
#  paste0("cost.fn_I(0.5 * log(q", lead.zero(2:J), ")^2) = S.n.H.", lead.zero(2:J), "_log(w", #lead.zero(2:N),")")
#)
#}
# Above we never correct - we dont have that restriction since w00 does not appear in the  q00 cross terms

# REFERENCE: Kumbhaker & Lovell 2000, p. 223 for parameter restrictions and 178, footnote 10 of Reinhard and Thijssen

# ln.lm.E.vars <- names(coef(test.lm))
# ln.lm.E.vars <- names(coef(linear.terms.for.adding.up))
# ln.lm.E.vars <- names(coef(linear.terms.for.adding.up))
# add up across 2nd number (w) for kappa

ln.lm.E.vars <- attr(terms(as.formula(
  paste0("ln.E.data ~ ",  gsub("[(]Intercept[)] .", "",  linear.terms.for.adding.up))
  ), keep.order=TRUE), "term.labels")


#for ( i in grep("log[(]w[0-9][0-9][)]:log[(]q[0-9][0-9][)]", ln.lm.E.vars) ) {
#  ln.lm.E.vars[i] <- paste0(gsub(".+:", "", ln.lm.E.vars[i]), ":", gsub(":.+", "", ln.lm.E.vars[i]) )
#}


# ln.c.linear

# REFERENCE: http://www.jstor.org/stable/2328171
# Single betas sum to 1
# Double betas sum to 0 in 1st dimension
# gammas sum to zero in 2nd (w[xx]) direction




# if (M>1) {

betas.single <- sort(ln.lm.E.vars[grepl("^log[(]w[0-9][0-9][)]$", ln.lm.E.vars)])

 ln.E.string <- str_replace_all(ln.E.string, "beta01", paste0("(-(", paste0(betas.single[-1], collapse=" + "), " - 1))" ) )
# OK, this reference may be wrong, but the code above is right: p. 4 of http://ageconsearch.umn.edu/bitstream/22027/1/sp03mo02.pdf

single.beta.restriction <- paste0( paste0("cost.fn_", betas.single, collapse=" + "), " = 1" )



# beta.input.adding.up <- ln.lm.E.vars[grepl("(I[(]0.5 . log[(]w[0-9][0-9][)].2[)])|(log[(]w[0-9][0-9][)].I[(]0.5 . log[(]w[0-9][0-9][)][)])" , ln.lm.E.vars )]

beta.input.adding.up <- ln.lm.E.vars[grepl("(I[(]0.5 . log[(]w[0-9][0-9][)].2[)])|(log[(]w[0-9][0-9][)].log[(]w[0-9][0-9][)])" , ln.lm.E.vars )]

# "log(w02):I(0.5 * log(w01))"
# "I(0.5 * log(w02)^2)"


m2 <- matrix("",  ncol=N, nrow=N)
lower.tri(m2)
m2[lower.tri(m2, diag=TRUE)] <- beta.input.adding.up
ind <- upper.tri(m2) 
m2[ind] <- t(m2)[ind] 
# Thanks to http://r.789695.n4.nabble.com/Symmetric-matrix-td853754.html

# m2[m2 %in% names(coef(test.lm))[is.na(coef(test.lm))] ] <- NA
# m2[m2 %in% names(coef(singular.test.lm )[is.na(coef(singular.test.lm ))]) ] <- NA
m2[m2 %in% singular.terms ] <- NA



beta.adding.up.mat <- matrix(paste0("cost.fn_", m2), ncol=N, nrow=N)

# y02:I(0.5 * y04)
# I(0.5 * y02^2)

double.beta.restrictions <-
  paste("",
    apply(beta.adding.up.mat, 1, paste, collapse=" + " ),
  " = 0" )
  
double.beta.restrictions <- gsub("[+] cost.fn_NA", "", double.beta.restrictions)
double.beta.restrictions <- gsub("cost.fn_NA [+] ", "", double.beta.restrictions)



gamma.input.adding.up <- ln.lm.E.vars[grepl("y[0-9][0-9].log[(]w[0-9][0-9][)]" , ln.lm.E.vars )]

# "log(w02):I(0.5 * log(w01))"
# "I(0.5 * log(w02)^2)"

gamma.adding.up.mat <- matrix(paste0("cost.fn_", gamma.input.adding.up), nrow=M, ncol=N)

for ( i in singular.terms) {
  gamma.adding.up.mat <- gsub(i , "NA", gamma.adding.up.mat, fixed=TRUE)
}


# y02:I(0.5 * y04)
# I(0.5 * y02^2)

double.gamma.restrictions <-
  paste("",
    apply(gamma.adding.up.mat, 1, paste, collapse=" + " ),
  " = 0" )
  
double.gamma.restrictions <- gsub("[+] cost.fn_NA", "", double.gamma.restrictions)
double.gamma.restrictions <- gsub("cost.fn_NA [+] ", "", double.gamma.restrictions)
double.gamma.restrictions <- gsub("cost.fn_NA", "", double.gamma.restrictions)

double.gamma.restrictions <- double.gamma.restrictions[grepl("cost.fn", double.gamma.restrictions)]


kappa.input.adding.up <- ln.lm.E.vars[grepl("log[(]w[0-9][0-9][)].log[(]q[0-9][0-9][)]" , ln.lm.E.vars )]

kappa.adding.up.mat <- matrix(paste0("cost.fn_", kappa.input.adding.up), nrow=J, ncol=N)

for ( i in singular.terms) {
  kappa.adding.up.mat <- gsub(i , "NA", kappa.adding.up.mat, fixed=TRUE)
}

double.kappa.restrictions <-
  paste("",
    apply(kappa.adding.up.mat, 1, paste, collapse=" + " ),
  " = 0" )
  
double.kappa.restrictions <- gsub("[+] cost.fn_NA", "", double.kappa.restrictions)
double.kappa.restrictions <- gsub("cost.fn_NA [+] ", "", double.kappa.restrictions)
double.kappa.restrictions <- gsub("cost.fn_NA", "", double.kappa.restrictions)

double.kappa.restrictions <- double.kappa.restrictions[grepl("cost.fn", double.kappa.restrictions)]


# }

lm.param.restrictions <- c(lm.param.restrictions, single.beta.restriction , double.beta.restrictions, double.gamma.restrictions, double.kappa.restrictions    )



# TODO: say in paper that restrictions ensure that null hypothesis means that the shadow is same as orig


# lm.param.restrictions <- lm.param.restrictions[-c(31, 33)]


linear.sur.est <- systemfit( S.n.H, "SUR", restrict.matrix = lm.param.restrictions,  maxit = 5000 )










# Below, need to deal with the fact that log(w03):log(q01)  is reversed


S.n.H.tech.test <- list()

for ( n in 1:N) {
 S.n.H.tech.test[[n]] <- as.formula(
   paste0( "I( (x", lead.zero(n), " * ", "w", lead.zero(n), ")/exp(ln.E.data)) ~ tech:(",
    paste0("log(w", lead.zero(1:N), ")", collapse=" + "), " + ", 
    paste0("y", lead.zero(1:M), collapse=" + "), ")"
   )
  )
  names(S.n.H.tech.test)[n] <- paste0("S.n.H.", lead.zero(n))
}

S.n.H.tech.test[[1]] <- NULL

S.n.H.tech.test[[length(S.n.H.tech.test)+1]] <- as.formula(paste0("ln.E.data ~ tech:(", S.n.H.cost.fn.string, ")"))

names(S.n.H.tech.test)[length(S.n.H.tech.test)] <- "cost.fn"

tech <- as.factor(groups)



linear.sur.est.tech.test <- systemfit( S.n.H.tech.test, "SUR", restrict.matrix = lm.param.restrictions,  maxit = 5000 )

# tech <- as.factor(rep(1, length(groups))
# linear.sur.est.tech.test <- systemfit( S.n.H.tech.test[1:2], "SUR",  maxit = 5000 )
# linear.sur.est.tech.test <- lm( S.n.H.tech.test[[1]])


# I think I can check the linear equality hypothesis with UCLA's R SUR page
# Also would have to have a separate intercept for cost fn, which just means a stand-alone dummy var

# Ok, actually I am not convinced that this is a good idea. Maybe we need to just delete interaction for the groups that it doesnt apply for


#for (i in 1:length(S.n.H)) {
#  lm.tech.check <- lm( S.n.H.tech.test[[i]] )
#  singular.terms <- names(coef(lm.tech.check )[is.na(coef(lm.tech.check ))])
# }  



lm.tech.check <- lm( S.n.H.tech.test[[length(S.n.H.tech.test)]] )
tech.check.singular.terms <- names(coef(lm.tech.check )[is.na(coef(lm.tech.check ))])

S.n.H.cost.fn.string.tech.test.terms <- strsplit(S.n.H.cost.fn.string, "+", fixed=TRUE)[[1]]

# I'm not sure whether not taking out w01 from this will result in a singularity:
S.n.H.cost.fn.string.tech.test.terms <- paste0("tech", 
  rep( 1:length(unique(groups)), each=length(S.n.H.cost.fn.string.tech.test.terms)),
   ":", gsub(" ", "", S.n.H.cost.fn.string.tech.test.terms))
# Also could be a problem with terms switching places

S.n.H.cost.fn.string.tech.test.terms <- S.n.H.cost.fn.string.tech.test.terms[ ! S.n.H.cost.fn.string.tech.test.terms %in% gsub(" ", "", tech.check.singular.terms) ]

S.n.H.cost.fn.string.tech.test <- paste0(S.n.H.cost.fn.string.tech.test.terms, collapse=" + ")

tech1 <- ifelse(tech==1, 1, 0)
tech2 <- ifelse(tech==2, 1, 0)
tech3 <- ifelse(tech==3, 1, 0)
tech4 <- ifelse(tech==4, 1, 0)
tech5 <- ifelse(tech==5, 1, 0)
tech6 <- ifelse(tech==6, 1, 0)

lm.tech.check.2 <- lm(
  as.formula(paste0("ln.E.data ~ tech + ", S.n.H.cost.fn.string.tech.test))
)
# Oll Korrect!

# I believe that all that we have to worry about below is double-gamma parameters

S.n.H.tech.test <- list()

for ( n in 1:N) {
 S.n.H.tech.test[[n]] <- paste0(
    paste0("log(w", lead.zero(1:N), ")", collapse=" + "), " + ", 
    paste0("y", lead.zero(1:M), collapse=" + ")
   )
  names(S.n.H.tech.test)[n] <- paste0("S.n.H.", lead.zero(n))
  S.n.H.tech.test[[n]] <- strsplit(S.n.H.tech.test[[n]], " + ", fixed=TRUE)[[1]]
  
  S.n.H.tech.test[[n]] <- paste0("tech", 
  rep( 1:length(unique(groups)), each=length(S.n.H.tech.test[[n]])),
   ":", gsub(" ", "", S.n.H.tech.test[[n]])) 
   
   gammas.cost.share.can.have <- gsub("[:]log[(]w[0-9][0-9][)]", "", 
   S.n.H.cost.fn.string.tech.test.terms[
     grepl(paste0("log(w", lead.zero(n), ")"), S.n.H.cost.fn.string.tech.test.terms, fixed=TRUE)])
   
   S.n.H.tech.test[[n]] <- S.n.H.tech.test[[n]][ grepl("log", S.n.H.tech.test[[n]]) |
     S.n.H.tech.test[[n]] %in% gammas.cost.share.can.have ]
     
   S.n.H.tech.test[[n]]  <- c(paste0("tech", 1:length(unique(groups))), S.n.H.tech.test[[n]] )
     
   S.n.H.tech.test[[n]]  <- paste0("I( (x", lead.zero(n), 
   " * ", "w", lead.zero(n), ")/exp(ln.E.data)) ~ ",
   paste0(S.n.H.tech.test[[n]], collapse=" + "), " - 1" )
   # Negative 1 gives us no intercept

}

S.n.H.tech.test[[1]] <- NULL

# gsub("[:]log[(]w04[)]", "", "tech5:y08:log(w04)" )
# "tech1:y02"

S.n.H.tech.test[[length(S.n.H.tech.test) + 1]] <- paste0("ln.E.data ~ tech + ", S.n.H.cost.fn.string.tech.test)

for ( i in 1:length(S.n.H.tech.test)) {
  S.n.H.tech.test[[i]] <- as.formula(S.n.H.tech.test[[i]])
}


test.test <- linear.sur.est.tech.test <- systemfit( S.n.H.tech.test, "SUR", maxit = 5000 )
# Yeshhh!!
# Ok, now we just need the restriction  vector
# probably should replace the explicit intercept with tech1

# Computing marginal products of inputs: 
# http://ageconsearch.umn.edu/bitstream/29726/1/16020001.pdf
# www.econjournals.com/index.php/ijeep/article/download/123/99


# interactin vs. separate regressions: http://www3.nd.edu/~rwilliam/stats2/l51.pdf
# better source: http://www.uv.es/uriel/5%20Multiple%20regression%20analysis%20with%20qualitative%20information.pdf
# Also see this: http://forums.eviews.com/viewtopic.php?f=4&t=440
# <aybe I should have an honest-t0-God zero var for one of the technologies

# Probably a rigorous way to test for structural breaks, but seems like hell to implement: http://www.jstor.org/stable/4501997
# Well, maybe we could do the above. Also this is similar: http://sws1.bu.edu/perron/papers/dealing.pdf

# This may be what we need, but it is behind a paywall: http://www.tandfonline.com/doi/full/10.1080/13504851.2012.689105#preview

# This could help too: Seemingly Unrelated Regression Equations Models: Estimation and Inference

# Note: the main SUR function is not quadratic since second deriv is not always negative

summary( test.test <- lm(S.n.H.tech.test[[1]]))



# has to be a tech intercept for all cost share eqns






S.n.H.tech.test[[length(S.n.H.tech.test)+1]] <- as.formula(paste0("ln.E.data ~ tech + tech:(", S.n.H.cost.fn.string.tech.test, ")"))


S.n.H.tech.test <- list()

for ( n in 1:N) {
 S.n.H.tech.test[[n]] <- as.formula(
   paste0( "I( (x", lead.zero(n), " * ", "w", lead.zero(n), ")/exp(ln.E.data)) ~ tech:(",
    paste0("log(w", lead.zero(1:N), ")", collapse=" + "), ")"
   )
  )
  names(S.n.H.tech.test)[n] <- paste0("S.n.H.", lead.zero(n))
}

S.n.H.tech.test[[1]] <- NULL

S.n.H.cost.fn.string.tech.test.terms <- strsplit(S.n.H.cost.fn.string, "+", fixed=TRUE)[[1]]

S.n.H.cost.fn.string.tech.test <- S.n.H.cost.fn.string.tech.test.terms[!grepl("y[0-9][0-9]", strsplit(S.n.H.cost.fn.string.tech.test.terms, "+", fixed=TRUE))]

S.n.H.cost.fn.string.tech.test <- paste0(S.n.H.cost.fn.string.tech.test, collapse="+")

S.n.H.tech.test[[length(S.n.H.tech.test)+1]] <- as.formula(paste0("ln.E.data ~ tech + tech:(", S.n.H.cost.fn.string.tech.test, ")"))

names(S.n.H.tech.test)[length(S.n.H.tech.test)] <- "cost.fn"








# linear.sur.est <- systemfit( S.n.H[length(S.n.H)], "SUR", restrict.matrix = lm.param.restrictions[!grepl("S.n.H", lm.param.restrictions)] ,  maxit = 5000 )
# TODO: Trying to put only the cost function to avoid eliminating so many observations later

# summary(lm(S.n.H[[length(S.n.H)]]))



# kleinOls.M6.N4 <- systemfit( S.n.H, "SUR", restrict.matrix = lm.param.restrictions,  maxit = 5000  )


# Next:
# maybe impose adding-up restrictions on linear SUR
# Need to find a way to link linear SUR parameter names to nonlinear SUR
# Need to zap singular variables in nonlinear string
# search r-squared and translog


# names(coef(linear.sur.est))

# Ok, so thus 
























#grep(i, S.n.H.cost.fn.string)


#data( "Kmenta" )
#eqDemand <- consump ~ price + income
#eqSupply <- consump ~ price + farmPrice + trend
#system <- list( demand = eqDemand, supply = eqSupply )

#restrict <- c( "demand_income - supply_trend = 0",
#   "- demand_price + supply_price = 0.5", "goop = goopB" )
#fitols2b <- systemfit( system, data = Kmenta, restrict.matrix = restrict )
#print( fitols2b )

#list( Consumption = eqConsump, Investment = eqInvest,
#    PrivateWages = eqPrivWage )


# x*w/E
# TODO: pretty sure that this is cost share, but may want to double-check

#-1 for no intercept

# ln.E.string <- ln.c

# The problem is that the code below assumes that no symmetry restrictions have been imposed, so our string has to be re-initiated.





############# MUST START HERE when constructing nonlinear string



ln.c <- paste0("beta0 + ", ln.c.1, " + ", ln.c.2, " + (1/2) * (", ln.c.3, 
") + (1/2) * (", ln.c.4, ") + ", ln.c.5)



gamma.special<-c()
gamma.mat<-matrix(1:(M*N), nrow=M, ncol=N)
for ( i in 1:N) {
  gamma.special[i] <- paste0(
    paste0("gamma", do.call(paste0, M.N.dim), " * ", ym.wn[[1]])[gamma.mat[, i]],
    collapse=" + " )
}

beta.special<-c()
beta.mat<-matrix(1:(N*N), nrow=N, ncol=N)
for ( i in 1:N) {
  beta.special[i] <- paste0(
    paste0("beta", do.call(paste0, N.2.dim[2:1]) , " * ", gsub("[*] $", "", ln.sh.w.grid[[1]]))[beta.mat[, i]],
    collapse=" + " )
}


ln.E.2nd <- paste0( "log(" , 
  paste0(
  paste0(" (w", lead.zero(1:N), " / (w", lead.zero(1:N), " * theta", 
   lead.zero(1:N), ")) * ", 
   "(beta", lead.zero(1:N), " + ",
   gamma.special, " + ",
   beta.special, ")"),
  collapse=" + " ), ")" )
   
# ln.E <- paste0("test.fn <- function(X) {", ln.c, " + ", ln.E.2nd, "}")


#ln.E.data <- log(w01*x01 + w02*x02 + w03*x03 + w04*x04 + w05*x05 )
# ln.E.data <- log(w01*x01 + w02*x02 + w03*x03 + w04*x04 + 2  )

#test.nls <- nls(nls.formula.ln.E, trace=TRUE)

#ln.E.string <- ln.c

# ln.E.string <- ln.c
# TODO: Restore this to the below when it is go-time - DONE
 ln.E.string <- paste(ln.c, " + ", ln.E.2nd)

ln.E.string.before.symm <-  ln.E.string 
# ln.E.string.before.symm <-  paste(ln.c, " + ", ln.E.2nd)



library(stringr)

####################### IMPOSING SYMMETRY RESTRICTIONS



#replacements <- data.frame(greek=c("alpha", "beta", "gamma"), N=c(M,N,N), M=c(M,N,M))
#replacements <- replacements[1:2, ]
# so do not actually need for gamma

#for ( k in 1:nrow(replacements) ) {

#if (replacements$greek[k]=="alpha" & M==1) {next}

#symm <- ln.E.vars[grepl(paste0(replacements$greek[k], "[.]"), ln.E.vars) ]

#symm.mat<-matrix(paste0(replacements$greek[k], ".", apply(X=expand.grid(lead.zero(1:max(N,M)), lead.zero(1:max(N,M))), MARGIN=1, FUN=paste, collapse=".")), nrow=max(N,M), ncol=max(N,M))

# ok the below is trying to reverse it:
#symm.mat<-matrix(paste0(replacements$greek[k], ".", apply(X=expand.grid(lead.zero(1:max(N,M)), lead.zero(1:max(N,M))), MARGIN=1, FUN=paste, collapse=".")), nrow=max(N,M), ncol=max(N,M))

#symm.mat[upper.tri(symm.mat, diag = FALSE)] <- t(symm.mat)[upper.tri(symm.mat, diag = FALSE)]
#symm.mat<-symm.mat[1:replacements$N[k], 1:replacements$M[k]]

# data.frame(symm, c(symm.mat))

#for ( i in 1:length(symm)) {
#  ln.E.string <- str_replace_all(ln.E.string, symm[i], c(symm.mat)[i])
#}

#}

# It seems symmetry has already been imposed




####################### IMPOSING ADDING-UP RESTRICTIONS



ln.E.vars <- all.vars(as.formula(paste("ln.E.data ~", ln.E.string.before.symm)))
ln.E.vars <- ln.E.vars[ !grepl("(w[0-9])|(y[0-9])|(ln.E.data)", ln.E.vars ) ]
ln.E.vars <- sort(ln.E.vars)

if (M>1) {

betas.single <- sort(ln.E.vars[grepl("beta[0-9][0-9]", ln.E.vars)])

ln.E.string <- str_replace_all(ln.E.string, "beta01", paste0("(-(", paste0(betas.single[-1], collapse=" + "), " - 1))" ) )
# from p. 4 of http://ageconsearch.umn.edu/bitstream/22027/1/sp03mo02.pdf

beta.input.adding.up <- sort( ln.E.vars[grepl("beta[.][0-9][0-9]", ln.E.vars)] )

beta.adding.up.mat <-matrix(sort(beta.input.adding.up ), ncol=N)

beta.adding.up.mat[, 1] <-
  paste("(-(",
    apply(beta.adding.up.mat[, -1], 1, paste, collapse=" + " ),
  "))" )
  
# data.frame(alpha.input.adding.up, c(alpha.adding.up.mat))

for ( i in 1:length(beta.input.adding.up )) {
  ln.E.string <- str_replace_all(ln.E.string, beta.input.adding.up[i], c(beta.adding.up.mat)[i])
}


gamma.input.adding.up <- sort( ln.E.vars[grepl("gamma[.][0-9][0-9]", ln.E.vars)] )

gamma.adding.up.mat <-matrix(sort(gamma.input.adding.up ), ncol=M, byrow=FALSE)

gamma.adding.up.mat[1, ] <-
  paste("(-(",
    apply(gamma.adding.up.mat[-1, ], 2, paste, collapse=" + " ),
  "))" )
  
# data.frame(alpha.input.adding.up, c(alpha.adding.up.mat))

for ( i in 1:length(gamma.input.adding.up)) {
  ln.E.string <- str_replace_all(ln.E.string, gamma.input.adding.up[i], c(gamma.adding.up.mat)[i])
}


}




####################### IMPOSING SYMMETRY RESTRICTIONS again after adding-up restrictions



replacements <- data.frame(greek=c("alpha", "beta", "gamma"), N=c(M,N,N), M=c(M,N,M))
replacements <- replacements[1:2, ]
# so do not actually need for gamma

for ( k in 1:nrow(replacements) ) {

if (replacements$greek[k]=="alpha" & M==1) {next}

# ok the below is trying to reverse it:
symm.mat<-matrix(paste0(replacements$greek[k], ".", apply(X=expand.grid(lead.zero(1:max(N,M)), lead.zero(1:max(N,M))), MARGIN=1, FUN=paste, collapse=".")), nrow=max(N,M), ncol=max(N,M))

symm.mat[upper.tri(symm.mat, diag = FALSE)] <- t(symm.mat)[upper.tri(symm.mat, diag = FALSE)]
symm.mat<-symm.mat[1:replacements$N[k], 1:replacements$M[k]]


expanded.greeks.grid <- expand.grid(lead.zero(1:max(N,M)), lead.zero(1:max(N,M)))
expanded.greeks.grid <- data.frame(expanded.greeks.grid$Var2, expanded.greeks.grid$Var1)

symm.mat.2<-matrix(paste0(replacements$greek[k], ".", apply(X=expanded.greeks.grid, MARGIN=1, FUN=paste, collapse=".")), nrow=max(N,M), ncol=max(N,M))

symm.mat.2[upper.tri(symm.mat.2, diag = FALSE)] <- t(symm.mat.2)[upper.tri(symm.mat.2, diag = FALSE)]
symm.mat.2<-symm.mat.2[1:replacements$N[k], 1:replacements$M[k]]



if (replacements$greek[k]=="alpha") {
for ( i in 1:length(c(symm.mat.2))) {
  ln.E.string <- str_replace_all(ln.E.string, c(symm.mat.2)[i], c(symm.mat)[i])
}
}

if (replacements$greek[k]=="beta") {
for ( i in 1:length(c(symm.mat.2))) {
  ln.E.string <- str_replace_all(ln.E.string, c(symm.mat)[i], c(symm.mat.2)[i])
}
}


}


######### NOW ADDING COST SHARE EQUATIONS
####################################
####################################



gamma.special<-c()
gamma.mat<-matrix(1:(M*N), nrow=M, ncol=N)
for ( i in 1:N) {
  gamma.special[i] <- paste0(
    paste0("gamma", sort(do.call(paste0, M.N.dim[1:2])), " * ", ym.wn[[1]])[gamma.mat[, i]],
    collapse=" + " )
}

beta.special<-c()
beta.mat<-matrix(1:(N*N), nrow=N, ncol=N)
for ( i in 1:N) {
  beta.special[i] <- paste0(
    paste0("beta", do.call(paste0, N.2.dim[1:2]) , " * ", gsub("[*] $", "", ln.sh.w.grid[[1]]))[beta.mat[, i]],
    collapse=" + " )
}


S.n.divisor <- paste0( "" , 
  paste0(
  paste0(" (w", lead.zero(1:N), " / (w", lead.zero(1:N), " * theta", 
   lead.zero(1:N), ")) * ", 
   "(beta", lead.zero(1:N), " + ",
   gamma.special, " + ",
   beta.special, ")"),
  collapse=" + " ), "" )
   
# ln.E <- paste0("test.fn <- function(X) {", ln.c, " + ", ln.E.2nd, "}")

S.n.top <- paste0(" w", lead.zero(1:N), " / (w", lead.zero(1:N), " * theta", 
   lead.zero(1:N), ")" )





S.n <- list()

for ( n in 1:N) {
  betas.for.S.n <- paste0("beta.", lead.zero(n), ".", lead.zero(1:N) )
  gammas.for.S.n <- paste0("gamma.", lead.zero(1:M), ".", lead.zero(n))
 
 S.n[[n]] <- 
   paste0( "I( (x", lead.zero(n), " * ", "w", lead.zero(n), ")/exp(ln.E.data)) ~ (",
    "beta", lead.zero(n), " + ",
    paste0( betas.for.S.n, " * " , " log(w", lead.zero(1:N), " * ", "theta", lead.zero(1:N),  ")", collapse=" + "), " + ", 
    paste0(gammas.for.S.n, " * ", "y", lead.zero(1:N), collapse=" + "), " ) * (",
    S.n.top[n], ") / (", S.n.divisor, ")"
   )
  
  names(S.n)[n] <- paste0("S.n", lead.zero(n))
}


S.n[[1]] <- NULL

grepl('gamma.01.05', as.character(S.n[[1]][3]) )




for (j in 1:length(S.n) ) {

targ.S.n <- S.n[[j]]


targ.S.n.vars <- all.vars(as.formula(targ.S.n))
targ.S.n.vars <- targ.S.n.vars[ !grepl("(w[0-9])|(y[0-9])|(ln.E.data)", targ.S.n.vars ) ]
targ.S.n.vars <- sort(targ.S.n.vars)

if (M>1) {

betas.single <- sort(targ.S.n.vars[grepl("beta[0-9][0-9]", targ.S.n.vars)])

targ.S.n <- str_replace_all(targ.S.n, "beta01", paste0("(-(", paste0(betas.single[-1], collapse=" + "), " - 1))" ) )
# from p. 4 of http://ageconsearch.umn.edu/bitstream/22027/1/sp03mo02.pdf

# beta.input.adding.up <- sort( targ.S.n.vars[grepl("beta[.][0-9][0-9]", targ.S.n.vars)] )

#beta.adding.up.mat <-matrix(sort(beta.input.adding.up ), ncol=N)

#beta.adding.up.mat[, 1] <-
#  paste("(-(",
#    apply(beta.adding.up.mat[, -1], 1, paste, collapse=" + " ),
#  "))" )
  
# data.frame(alpha.input.adding.up, c(alpha.adding.up.mat))

for ( i in 1:length(beta.input.adding.up )) {
  targ.S.n <- str_replace_all(targ.S.n, beta.input.adding.up[i], c(beta.adding.up.mat)[i])
}


# gamma.input.adding.up <- sort( targ.S.n.vars[grepl("gamma[.][0-9][0-9]", targ.S.n.vars)] )

# gamma.adding.up.mat <-matrix(sort(gamma.input.adding.up ), ncol=M, byrow=FALSE)

# gamma.adding.up.mat[1, ] <-
#   paste("(-(",
#     apply(gamma.adding.up.mat[-1, ], 2, paste, collapse=" + " ),
#   "))" )
  
# data.frame(alpha.input.adding.up, c(alpha.adding.up.mat))

for ( i in 1:length(gamma.input.adding.up)) {
  targ.S.n <- str_replace_all(targ.S.n, gamma.input.adding.up[i], c(gamma.adding.up.mat)[i])
}


}




####################### IMPOSING SYMMETRY RESTRICTIONS again after adding-up restrictions



replacements <- data.frame(greek=c("alpha", "beta", "gamma"), N=c(M,N,N), M=c(M,N,M))
replacements <- replacements[1:2, ]
# so do not actually need for gamma

for ( k in 1:nrow(replacements) ) {

if (replacements$greek[k]=="alpha" & M==1) {next}

# ok the below is trying to reverse it:
symm.mat<-matrix(paste0(replacements$greek[k], ".", apply(X=expand.grid(lead.zero(1:max(N,M)), lead.zero(1:max(N,M))), MARGIN=1, FUN=paste, collapse=".")), nrow=max(N,M), ncol=max(N,M))

symm.mat[upper.tri(symm.mat, diag = FALSE)] <- t(symm.mat)[upper.tri(symm.mat, diag = FALSE)]
symm.mat<-symm.mat[1:replacements$N[k], 1:replacements$M[k]]


expanded.greeks.grid <- expand.grid(lead.zero(1:max(N,M)), lead.zero(1:max(N,M)))
expanded.greeks.grid <- data.frame(expanded.greeks.grid$Var2, expanded.greeks.grid$Var1)

symm.mat.2<-matrix(paste0(replacements$greek[k], ".", apply(X=expanded.greeks.grid, MARGIN=1, FUN=paste, collapse=".")), nrow=max(N,M), ncol=max(N,M))

symm.mat.2[upper.tri(symm.mat.2, diag = FALSE)] <- t(symm.mat.2)[upper.tri(symm.mat.2, diag = FALSE)]
symm.mat.2<-symm.mat.2[1:replacements$N[k], 1:replacements$M[k]]



if (replacements$greek[k]=="alpha") {
for ( i in 1:length(c(symm.mat.2))) {
  targ.S.n <- str_replace_all(targ.S.n, c(symm.mat.2)[i], c(symm.mat)[i])
}
}

if (replacements$greek[k]=="beta") {
for ( i in 1:length(c(symm.mat.2))) {
  targ.S.n <- str_replace_all(targ.S.n, c(symm.mat)[i], c(symm.mat.2)[i])
}
}


}


S.n[[j]] <- targ.S.n

}






####################################
####################################




singular.test.lm <-lm(orig.singular.model)
singular.terms <- names(coef(singular.test.lm )[is.na(coef(singular.test.lm ))])


# If we dont have <if> statement, then we will get something like this in ln.E.string:  * y01 * y0102.01 * y02 * y0103.01 * y03 * y0104.01 * y04 * y0105.01 * y05 * y0102.01 * y01 * y0202.02 * y02 * y0203.02 * y03 * y0204.02 * y04 * y0205.02 * y05 * y0203.01 
if (length(singular.terms)>0 ) {

singular.terms <- gsub("(I[(]0.5 . )|([)])", "", singular.terms )



for ( i in singular.terms) {
  singular.terms<-c(singular.terms, 
    paste0(gsub("y[0-9]+:", "", i), ":", gsub(":y[0-9]+", "", i)) )
}

singular.terms <- gsub(":", " . ", singular.terms )

# Ok, this will give an error if one of the crops never uses a particular input

#for ( i in singular.terms) {
#   ln.E.string <- 
#     gsub(paste0("alpha[.][0-9]{2}[.][0-9]{2} . ", i, " . "), "", ln.E.string)
#}

 singular.terms <- paste0("alpha.", gsub("( )|(y)", "", singular.terms) )

singular.terms <- c(singular.terms, 
  paste0("alpha", gsub("^alpha.[0-9][0-9]", "", singular.terms), 
    gsub("(alpha)|(.[0-9][0-9]$)", "", singular.terms)) )

for ( i in singular.terms) {
   ln.E.string <- 
     gsub(paste0("alpha[.][0-9]{2}[.][0-9]{2} . ", i, " . "), "", ln.E.string)
}

singular.terms <- paste0( gsub("( )|(y)", "", singular.terms) )

for ( i in singular.terms) {
   ln.E.string <- gsub(paste0(i, " . " ), "", ln.E.string)
   ln.E.string <- gsub(paste0(" . ", i ), "", ln.E.string)
   
   for ( j in 1:length(S.n) ) {
     S.n[[j]] <- gsub(paste0(i, " . " ), "", S.n[[j]])
     S.n[[j]] <- gsub(paste0(" . ", i ), "", S.n[[j]])
   }
   
   # Because the + could be before or after the term
}

}

all.vars(as.formula(paste("ln.E.data ~", ln.E.string)))

# TODO: Where is alpha.01.01? Nevermind, it disappeared with the adding up restrictions


linear.names <- names(coef(linear.sur.est))
linear.names <- gsub("cost.fn_", "", linear.names[grepl("cost.fn_", linear.names)])

lin.to.nonlin.crossref.df <- data.frame(linear=linear.names, nonlinear=linear.names, stringsAsFactors=FALSE)

for ( i in 1:M) {
  lin.to.nonlin.crossref.df$nonlinear <- str_replace_all(
    lin.to.nonlin.crossref.df$nonlinear,
    paste0("^y", lead.zero(i), "$"), paste0("alpha", lead.zero(i)))
}

for ( i in 1:N) {
  lin.to.nonlin.crossref.df$nonlinear <- str_replace_all(
    lin.to.nonlin.crossref.df$nonlinear,
    paste0("^log[(]w", lead.zero(i), "[)]$"), paste0("beta", lead.zero(i)))
}



for ( i in 1:M) {
  lin.to.nonlin.crossref.df$nonlinear <- str_replace_all(
    lin.to.nonlin.crossref.df$nonlinear,
    paste0("^I[(]0.5 . y", lead.zero(i), ".2[)]$"), paste0("alpha.", lead.zero(i), ".", lead.zero(i)))
    # paste0("^I[(] y", lead.zero(i), ".2[)]$")
}


#for ( i in 1:length( M.2.dim )) {
#  lin.to.nonlin.crossref.df$nonlinear <- str_replace_all(
#    lin.to.nonlin.crossref.df$nonlinear,
#    paste0(y.perm[[1]][i], ":", y.perm[[2]][i]), 
#    paste0("alpha", M.2.dim[i]) )
#}

for ( i in 1:length( M.2.dim )) {
  lin.to.nonlin.crossref.df$nonlinear <- str_replace_all(
    lin.to.nonlin.crossref.df$nonlinear,
    paste0(y.perm[[2]][i], ":", y.perm[[1]][i]), 
    paste0("alpha", M.2.dim[i]) )
    # paste0(y.perm[[2]][i], ":", "I[(]0.5 . ", y.perm[[1]][i], "[)]"), 
}


# y01:I(0.5 * y02)
#paste0("^I[(]0.5 . log[(]w", lead.zero(i), "[)].I[(]0.5 . log[)]$")
#"log(w03):I(0.5 * log(w01))"






N.2.dim.fixed <- lapply(N.2.dim, FUN=gsub, pattern="[.]", replacement="")

#for ( i in 1:length( N.2.dim.fixed[[1]] )) {
#  lin.to.nonlin.crossref.df$nonlinear <- str_replace_all(
#    lin.to.nonlin.crossref.df$nonlinear,
#    paste0("log[(]w", N.2.dim.fixed[[1]][i], "[)]:log[(]w", N.2.dim.fixed[[2]][i], "[)]"), 
#    paste0("beta.", N.2.dim.fixed[[1]][i], ".", N.2.dim.fixed[[2]][i]) )
#}

for ( i in 1:length( N.2.dim.fixed[[1]] )) {
  lin.to.nonlin.crossref.df$nonlinear <- str_replace_all(
    lin.to.nonlin.crossref.df$nonlinear,
    paste0("log[(]w", N.2.dim.fixed[[1]][i], "[)]:log[(]w", N.2.dim.fixed[[2]][i], "[)]"), 
    paste0("beta.", N.2.dim.fixed[[1]][i], ".", N.2.dim.fixed[[2]][i]) )
    #    paste0("log[(]w", N.2.dim.fixed[[1]][i], "[)]:I[(]0.5 . log[(]w", N.2.dim.fixed[[2]][i], "[)][)]"), 
}


# I(0.5 * log(w01)^2)

for ( i in 1:M ) {
  lin.to.nonlin.crossref.df$nonlinear <- str_replace_all(
    lin.to.nonlin.crossref.df$nonlinear,
    paste0("I[(]0.5 . log[(]w", lead.zero(i), "[)].2[)]"), 
    paste0("beta.", lead.zero(i), ".", lead.zero(i)) )
}

M.N.dim.fixed <- lapply(M.N.dim, FUN=gsub, pattern="[.]", replacement="")

for ( i in 1:length( M.N.dim.fixed[[1]] )) {
  lin.to.nonlin.crossref.df$nonlinear <- str_replace_all(
    lin.to.nonlin.crossref.df$nonlinear,
    paste0("y", M.N.dim.fixed[[1]][i], ":log[(]w", M.N.dim.fixed[[2]][i], "[)]"), 
    paste0("gamma.", M.N.dim.fixed[[1]][i], ".", M.N.dim.fixed[[2]][i]) )
}

lin.to.nonlin.crossref.df$nonlinear[lin.to.nonlin.crossref.df$nonlinear=="(Intercept)"] <- "beta0"

# crossref done


ln.E.vars <- lin.to.nonlin.crossref.df$nonlinear



# ln.E.vars <- all.vars(nls.formula.ln.E)
#ln.E.vars <- all.vars(as.formula(paste("ln.E.data ~", ln.E.string)))
#ln.E.vars <- ln.E.vars[ !grepl("(w[0-9])|(y[0-9])|(ln.E.data)", ln.E.vars ) ]
ln.E.start.vals <- vector(mode="numeric", length=length(ln.E.vars))
ln.E.start.vals <- coef(linear.sur.est)[grepl("cost.fn_", names(coef(linear.sur.est)))]
# ln.E.vars <- sort(ln.E.vars)
names(ln.E.start.vals) <- ln.E.vars
#ln.E.start.vals[grepl("(beta)|(gamma)|(alpha)", names(ln.E.start.vals))] <- 5
#ln.E.start.vals[grepl("theta", names(ln.E.start.vals))] <- 1

# NOTE: Would need to fix the below if M=1
# ln.E.start.vals <- ln.E.start.vals[!grepl("(beta01)|(beta....01)|(gamma....01)", 
#   names(ln.E.start.vals))]


# This is to handle the fact that some of these drop out with adding-up restrictions:
ln.E.start.vals <- ln.E.start.vals[!grepl("(alpha.03.01)|(beta01)|(beta....01)|(gamma....01)", 
  names(ln.E.start.vals))]
# alpha.03.01 <- 0

#TODO: for now we will kill alpha.03.01 since it only pertains to one observation and it will screw up gradient



# theta.starts <- c(.75, 1, 1.25)
 
#theta.starts <- c(.8, 1, 1.2)

theta.starts <- rep(1, times=N-1)
names(theta.starts) <- paste0("theta", lead.zero(1:(N-1)))
theta04 <- 1

# Now making manure numeraire and changing to price translation:
# theta.starts <- rep(0, times=N-1)
# names(theta.starts) <- paste0("theta", lead.zero(c(1,2,4)))
# theta03 <- 0



ln.E.start.vals <-c(ln.E.start.vals, theta.starts)


first.line <- paste0( "args <- c(\"", paste(names(ln.E.start.vals), sep="\", \"", collapse="\", \""), "\")\nfor ( i in 1:length(args)) { assign(args[i], x[i])} ; ")

eval(parse(text=paste0("mod.predicted <- function(x) {", first.line, 
"  ret <- ", ln.E.string, "; ifelse(is.finite(ret), ret, 10^300) }")))

mod.predicted(ln.E.start.vals)

args.list <- list(x01=x01, x02=x02, x03=x03, x04=x04, x05=x05, w01=w01, w02=w02, w03=w03,
 w04=w04, w05=w05, y01=y01, y02=y02, y03=y03, y04=y04, y05=y05, y06=y06, y07=y07, 
 y08=y08, y09=y09, y10=y10, y11=y11, y12=y12, ln.E.data=ln.E.data)


keep.safe.args.list <- list(x01=x01, x02=x02, x03=x03, x04=x04, x05=x05, w01=w01, w02=w02, w03=w03,
 w04=w04, w05=w05, y01=y01, y02=y02, y03=y03, y04=y04, y05=y05, y06=y06, y07=y07, 
 y08=y08, y09=y09, y10=y10, y11=y11, y12=y12, ln.E.data=ln.E.data)
# TODO: very important
# rm( list=names(keep.safe.args.list))



ln.E.2.substring <- substr(ln.E.string, 
  regexpr("log( (w01 / (w01 * theta01))", ln.E.string, fixed=TRUE),
  nchar(ln.E.string)
  )
  
ln.E.2.substring <- substr(ln.E.2.substring, 5, nchar(ln.E.2.substring )-1)
# Need: args.list <- c(list(x=ln.E.start.vals), args.list)

eval(parse(text=paste0("mod.predicted.substring <- function(x, ", paste0(names(args.list)[-1], collapse=", "), ") {", first.line, 
"  ret <- ", ln.E.2.substring, "; ifelse(is.finite(ret), ret, 10^300) }")))



do.call(mod.predicted.substring, args.list)


# args.list <- c(list(x=ln.E.start.vals), args.list)
# ALSO NEED: mod.predicted  created

theta.grid <- expand.grid(seq(.75, 1.25, by=.05), seq(.75, 1.25, by=.05), seq(.75, 1.25, by=.05) )

theta.grid.mat <- matrix(rep(args.list[[1]][-((length(args.list[[1]])-2):length(args.list[[1]]))], times=nrow(theta.grid)), ncol=nrow(theta.grid))

theta.grid.mat <- rbind(theta.grid.mat, t(as.matrix(theta.grid)))

theta.grid.test <- apply(theta.grid.mat, 2, FUN=
  function(x) {args.list[[1]] <- x;  
  sum((do.call(mod.predicted, args.list) - args.list$ln.E.data)^2) 
  } )
  
theta.grid[ is.finite(theta.grid.test), ]

theta.grid.test.table <- apply(theta.grid.mat, 2, FUN=
  function(x) {args.list[[1]] <- x;  
  sum(is.finite((do.call(mod.predicted, args.list) - args.list$ln.E.data)^2) )
  } )
  
#summary(theta.grid.test.table)
#hist(theta.grid.test.table)

theta.grid.which.infinite <- apply(theta.grid.mat, 2, FUN=
  function(x) {args.list[[1]] <- x;  
  which(!is.finite((do.call(mod.predicted, args.list) - args.list$ln.E.data)^2) )
  } )
  
theta.exclude <- unique(unlist(theta.grid.which.infinite))


exclude.these <- unique(c(theta.exclude)) #,  which(mod.predicted(ln.E.start.vals) < 100 ) ))

length(exclude.these)

args.list <- lapply(args.list, FUN=function(x) {
  x[-exclude.these]
  }
)


args.list <- c(list(x=ln.E.start.vals), args.list)

# THE above line is actually pretty crucial
# NOTE: WE NEED TO READ UP TO HERE

eval(parse(text=paste0("mod.predicted <- function(x, ", paste0(names(args.list)[-1], collapse=", "), ") {", first.line, 
"  ret <- ", ln.E.string, "; ifelse(is.finite(ret), ret, 10^300) }")))

do.call(mod.predicted, args.list)

table(do.call(mod.predicted, args.list) < 10^100)

which(do.call(mod.predicted, args.list) > 10^100)

# Ok, so we got rid of 4 observations that were causing problems

# library(codetools); findGlobals(mod.predicted)

# TODO: would arbitrarily killing alpha.00.00 due to singularity mess up our adding-up restrictions?

do.call(function(x){args.list[[1]] <- x;  sum((do.call(mod.predicted, args.list) - args.list$ln.E.data)^2) }, list(x=unlist(test)[1:length(ln.E.start.vals)]))

do.call(function(x){args.list[[1]] <- x;  do.call(mod.predicted, args.list) - args.list$ln.E.data }, list(x=unlist(test)[1:length(ln.E.start.vals)]))

zero.grad <- numDeriv::grad(function(x){args.list[[1]] <- x;  sum((do.call(mod.predicted, args.list) - args.list$ln.E.data)^2) }, unlist(test)[1:length(ln.E.start.vals)])
# sum((do.call(mod.predicted, args.list) - args.list$ln.E.data)^2)


sum((predict(linear.sur.est)$cost.fn.pred - args.list$ln.E.data)^2)
sum((resid(linear.sur.est)$cost.fn)^2)
# TODO: not sure why the above two expressions are not the same

do.call(mod.predicted, args.list) -  predict(linear.sur.est)$cost.fn.pred

mod.predicted(ln.E.start.vals) - predict(linear.sur.est)$cost.fn.pred

# TODO: test: get to min when only really OLS

sum((predict(linear.sur.est)$cost.fn.pred - args.list$ln.E.data)^2)



#function.text <- llf.creator.fn(12,5,1)


# x01.store <-x01; rm(x01)   ;     
# x01 <- x01.store



##########################
##########################
##########################
##########################
##########################
##########################

# Changing to price translation - DONT DO THIS if dont want the additive model

for ( j in 1:length(S.n) ) {
  S.n[[j]] <- gsub("* theta", "+ theta", S.n[[j]], fixed=TRUE)
}

ln.E.string <- gsub("* theta", "+ theta", ln.E.string, fixed=TRUE)


ln.E <- paste0("nls.formula.ln.E <- ln.E.data ~ ", ln.E.string)
eval(parse(text=ln.E))

for ( j in 1:length(S.n) ) {
  S.n[[j]] <- as.formula(S.n[[j]])
}



full.system.ls <- S.n
full.system.ls[[length(full.system.ls)+1]] <- nls.formula.ln.E
names(full.system.ls)[length(full.system.ls)] <-"cost.fn"

# TODO: Do we have to consider the environment when forming the formula?


try.nls.group.2.SUR.fix <- nlsystemfit(method="SUR", eqns= full.system.ls, startvals=ln.E.start.vals, data=as.data.frame(args.list[-1]), print.level=2, maxiter=100000)

try.nls.group.2.SUR <- nlsystemfit(method="SUR", eqns= full.system.ls, startvals=try.params, data=as.data.frame(args.list[-1]), print.level=2, maxiter=100000)

do.call(function(x){args.list[[1]] <- x;  sum((do.call(mod.predicted, args.list) - args.list$ln.E.data)^2) }, list(x=unlist(test)[1:length(ln.E.start.vals)]))




eval(parse(text=paste0("mod.predicted <- function(x, ", paste0(names(args.list)[-1], collapse=", "), ") {", first.line, 
"  ret <- ", ln.E.string, "; ifelse(is.finite(ret), ret, 10^300) }")))




sum((args.list$ln.E.data - mean(args.list$ln.E.data))^2)

sum((predict(linear.sur.est)$cost.fn.pred - args.list$ln.E.data)^2)

do.call(function(x){args.list[[1]] <- x;  sum((do.call(mod.predicted, args.list) - args.list$ln.E.data)^2) }, list(x=try.nls.group.2.SUR$b))

do.call(function(x){args.list[[1]] <- x;  do.call(mod.predicted, args.list)  }, list(x=try.nls.group.2.SUR$b))


linear.sur.est


ok, it just said, "Successive iterates within tolerance.
Current iterate is probably solution." quite quickly ofter the try.params. probably having to do with setting up the hessian at teh beginning or something

# Wow, taking a subset is much faster out of the starting line, probably because of fewer parameters
# The output mix will influence input mix - does the model capture this?
# Stuff is messed up again - somehow the gammas linear restriction is messed up


algo.analysis.txt<-readLines("/Users/travismcarthur/Desktop/Metrics (637)/Final paper/minimization algorithm analysis.txt")

algo.analysis.txt <- as.numeric( gsub("[1] ", "", algo.analysis.txt[grep("Function Value", algo.analysis.txt ) + 1 ], fixed=TRUE) )

# Of course, the below analysis assumes that the same step length is applied each time
plot(algo.analysis.txt[-(1:100)], type="l")
plot(diff(algo.analysis.txt[-(1:100)], differences = 1), type="l")
plot(diff(algo.analysis.txt[-(1:100)], differences = 2), type="l")
plot(diff(algo.analysis.txt[-(1:100)], differences = 3), type="l")
plot(diff(algo.analysis.txt[-(1:100)], differences = 4), type="l")
plot(diff(algo.analysis.txt[-(1:100)], differences = 5), type="l")
plot(diff(algo.analysis.txt[-(1:100)], differences = 6), type="l")

The "breathing time" seems to be a function of the number of observations. Maybe they are doing BHH or a variant


try.nls.translation.SUR <- nlsystemfit(method="SUR", eqns= full.system.ls, startvals=ln.E.start.vals, data=as.data.frame(args.list[-1]), print.level=2)



try.nls.off.thetas <- nlsystemfit(method="SUR", eqns= full.system.ls, startvals=ln.E.start.vals, data=as.data.frame(args.list[-1]), print.level=2)
# maxiter=10000

# save(file=paste0(work.dir, "try-nls.Rdata"), try.nls)

try.nls.2 <- nlsystemfit(method="SUR", eqns= full.system.ls, startvals=ln.E.start.vals, data=as.data.frame(args.list[-1]), print.level=2, maxiter=10000)



# WARNING: Non-identification is due to adding-up restriction
# TODO: change in performance may be due to dropping an observation

zero.grad <- numDeriv::grad(function(x){args.list[[1]] <- x;  sum((do.call(mod.predicted, args.list) - args.list$ln.E.data)^2) }, test$par)

numDeriv::grad(function(x){args.list[[1]] <- x;  sum((do.call(mod.predicted, args.list) - args.list$ln.E.data)^2) }, ln.E.start.vals)

# install.packages("optfntools",repos="http://r-forge.r-project.org", type = "source")
library(optfntools)

#install.packages("optimx")
library("optimx")

test <- optimx(ln.E.start.vals, function(x){args.list[[1]] <- x;  sum((do.call(mod.predicted, args.list) - args.list$ln.E.data)^2) },
      method = "BFGS", control = list(trace=6, maxit=160, save.failures = TRUE))
      
      
test.2 <- optimx(coef(fm1DNase1), function(x){args.list[[1]] <- x;  sum((do.call(mod.predicted, args.list) - args.list$ln.E.data)^2) },
      method ="BFGS" , control = list(trace=6, maxit=1000000, save.failures = TRUE))

save.failures = TRUE

fm1DNase1 <- nlsLM(nls.formula.ln.E, data=as.data.frame(args.list[-1]),
start=unlist(test)[1:length(ln.E.start.vals)], trace=TRUE, control=list(factor=.01, maxiter=5000, maxfev=2147483647))


xx <- ln.E.start.vals

test <- optim(ln.E.start.vals, function(x){args.list[[1]] <- x;  sum((do.call(mod.predicted, args.list) - args.list$ln.E.data)^2) },
      method = "CG", control = list(trace=6, maxit=1000000))

c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN",
                 "Brent")
# 13031.75
# TODO: May be some scaling issues
# Scaling help: http://rsnippets.blogspot.com/2014/01/tuning-optim-with-parscale.html

numDeriv::grad(function(x){args.list[[1]] <- x;  sum((do.call(mod.predicted, args.list) - args.list$ln.E.data)^2) }, test$par)


# What iterated SUR minimizes:
# "Iterated SUR is equivalent to a maximum likelihood estimation. Maximizing the 
# likelihood value is equivalent to minimizing the determinant of the residual 
# covariance matrix."
# https://stat.ethz.ch/pipermail/r-help/2004-November/061845.html



try.nls <- nlsystemfit(eqns= list(nls.formula.ln.E ), startvals=ln.E.start.vals, data=as.data.frame(args.list[-1]), print.level=2)


# TODO: must set one of the thetas to one always



test.nls <- nls(nls.formula.ln.E, start=ln.E.start.vals, trace=TRUE,
  data=as.data.frame(args.list[-1]))


test.nls <- nls(nls.formula.ln.E, start=coef(fm1DNase1), trace=TRUE,
  data=as.data.frame(args.list[-1]))


test.nls <- nls(nls.formula.ln.E, start=ln.E.start.vals, trace=TRUE, subset=include.these)
# , algorithm="plinear")


# RSS = 2.10365e+09
# for linear model, RSS= 2491056





test.nls <- nls(nls.formula.ln.E, start=test$par, trace=TRUE,
  data=as.data.frame(args.list[-1]))
  
  , algorithm="plinear")


library(minpack.lm)
### Examples from 'nls' doc ###
fm1DNase1 <- nlsLM(nls.formula.ln.E, data=as.data.frame(args.list[-1]),
start=ln.E.start.vals, trace=TRUE, control=list(factor=.01, maxiter=5000, maxfev=2147483647))


fm1DNase1 <- nlsLM(nls.formula.ln.E, data=as.data.frame(args.list[-1]),
start=test$par, trace=TRUE, control=list(maxiter=5000, maxfev=2147483647))

# Maybe we have something that is not identified in the nonlinear model

library(DEoptim)


mod.for.DEOoptim <- function(x) {
  sum((do.call(mod.predicted, c(list(x=x), args.list[-1])) - args.list$ln.E.data)^2 )
}

mod.for.DEOoptim(args.list[[1]])



for ( j in 1:20) {

init.mat<-matrix(0, nrow=10*length(args.list[[1]]), ncol=length(args.list[[1]]))

init.values <- args.list[[1]]

# init.values <- best.nls.params

init.mat[1,] <- init.values

# pa

for (i in 2:nrow(init.mat)) {
  init.mat[i,] <- jitter(init.values, amount=.1)
}

summary(jitter(args.list[[1]], amount=.1) - args.list[[1]])

# library(compiler)
# DEoptim.cmp <- cmpfun(DEoptim)
# ln.E.start.vals

ss <- DEoptim(mod.for.DEOoptim, lower= -abs(init.values) * 0.2, upper=abs(init.values) * 2,
              control=list(trace=TRUE, itermax=100, initialpop=init.mat))


pa.6 <- ss$optim$bestmem

try(fm1DNase2 <- nlsLM(nls.formula.ln.E, data=as.data.frame(args.list[-1]),
start=ss$optim$bestmem, trace=TRUE, control=list(maxiter=5000, maxfev=2147483647)))

cat("\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n", j, "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n")

}



summary(ss$optim$bestmem -best.nls.params)

test.nls <- nls(nls.formula.ln.E, start=ss$optim$bestmem, trace=TRUE,
  data=as.data.frame(args.list[-1]))
  
for ( i in 1:3) {
try(lm())
cat("mmmm")
}


nls::nlsModel

linear.sur.est

sum(resid(linear.sur.est)^2)


# http://stackoverflow.com/questions/9692677/how-to-find-good-start-values-for-nls-function



sum( (args.list$ln.E.data - mean(args.list$ln.E.data))^2 )








gsub("[*] theta[0-9]+", "", ln.E.string)

all.vars(as.formula(paste("ln.E.data ~", gsub("[*] theta[0-9]+", "", ln.E.string))))






gammas.mat<-matrix(paste0("gamma.", apply(X=expand.grid(lead.zero(1:max(N,M)), lead.zero(1:max(N,M))), MARGIN=1, FUN=paste, collapse=".")), nrow=max(N,M), ncol=max(N,M))
gammas.mat[upper.tri(gammas.mat, diag = FALSE)] <- t(gammas.mat)[upper.tri(gammas.mat, diag = FALSE)]
gammas.mat<-gammas.mat[1:J, 1:M]



first.line <- paste0( "args <- c(\"", paste(ln.E.vars, sep="\", \"", collapse="\", \""), "\")\nfor ( i in 1:length(args)) { assign(args[i], x[i])} ; ")

eval(parse(text=paste0("mod.predicted <- function(x) {", first.line, 
"  ret <- ", ln.E.string, "; ifelse(is.finite(ret), ret, 10^300) }")))

mod.predicted(ln.E.start.vals)


# ln.E.vars <- all.vars(nls.formula.ln.E)
ln.E.vars <- all.vars(as.formula(paste("ln.E.data ~", ln.E.string)))
ln.E.vars <- ln.E.vars[ !grepl("(w[0-9])|(y[0-9])|(ln.E.data)", ln.E.vars ) ]
ln.E.start.vals <- vector(mode="numeric", length=length(ln.E.vars))
ln.E.vars <- sort(ln.E.vars)
names(ln.E.start.vals) <- ln.E.vars
ln.E.start.vals[grepl("(beta)|(gamma)|(alpha)", names(ln.E.start.vals))] <- 5
ln.E.start.vals[grepl("theta", names(ln.E.start.vals))] <- 1
ln.E.start.vals

test.nls <- nls(nls.formula.ln.E, start=ln.E.start.vals, trace=TRUE, algorithm="plinear")

# install.packages("minpack.lm")
library(minpack.lm)
### Examples from 'nls' doc ###
fm1DNase1 <- nlsLM(nls.formula.ln.E, start=ln.E.start.vals, trace=TRUE, control=list(maxiter=5000, maxfev=2147483647) )


library(DEoptim)

mod <- function(x) x[1] + x[2]*x[3]^ExponCycles
fun <- function(x) sum((ExponValues-mod(x))^2)

ss <- DEoptim(fun, lower=rep(0,3), upper=c(10e7, 10, 10),
              control=list(trace=FALSE))

pa <- ss$optim$bestmem


eval(parse(text=paste0("mod <- function() { ", ln.c, " + ", ln.E.2nd, "}")))
fun <- function() sum((ln.E.data - mod())^2)









#### start here


first.line <- paste0( "args <- c(\"", paste(ln.E.vars, sep="\", \"", collapse="\", \""), "\")\nfor ( i in 1:length(args)) { assign(args[i], x[i])} ; ")

eval(parse(text=paste0("mod <- function(x) {", first.line, 
"  ret <- sum((ln.E.data - ", ln.E.string, ")^2); ifelse(is.finite(ret), ret, 10^300) }")))


#fun <- function() sum((ln.E.data - mod())^2)

ln.E.low.vals <- vector(mode="numeric", length=length(ln.E.vars))
names(ln.E.low.vals) <- ln.E.vars
ln.E.low.vals[grepl("(beta)|(gamma)|(alpha)", names(ln.E.low.vals))] <- .01
ln.E.low.vals[grepl("(alpha[.])|(gamma[.])", names(ln.E.low.vals))] <- -5
ln.E.low.vals[grepl("beta0", names(ln.E.low.vals))] <- - 10
ln.E.low.vals[grepl("theta", names(ln.E.low.vals))] <- .1
ln.E.high.vals <- vector(mode="numeric", length=length(ln.E.vars))
names(ln.E.high.vals) <- ln.E.vars
ln.E.high.vals[grepl("(beta)|(gamma)|(alpha)", names(ln.E.high.vals))] <- 5
ln.E.high.vals[grepl("theta", names(ln.E.high.vals))] <- 7



ln.E.low.vals <- vector(mode="numeric", length=length(ln.E.vars))
names(ln.E.low.vals) <- ln.E.vars
ln.E.low.vals[grepl("(beta)|(gamma)|(alpha)", names(ln.E.low.vals))] <- 10^-10
ln.E.low.vals[grepl("(alpha[.])|(gamma[.])", names(ln.E.low.vals))] <- -10^-10
ln.E.low.vals[grepl("beta0$", names(ln.E.low.vals))] <- - 10^3
ln.E.low.vals[grepl("theta", names(ln.E.low.vals))] <- 1
ln.E.high.vals <- vector(mode="numeric", length=length(ln.E.vars))
names(ln.E.high.vals) <- ln.E.vars
ln.E.high.vals[grepl("(beta)|(gamma)|(alpha)", names(ln.E.high.vals))] <- 2*10^-10
ln.E.high.vals[grepl("theta", names(ln.E.high.vals))] <- 1

ss <- DEoptim(mod, lower=ln.E.low.vals, upper=ln.E.high.vals,
              control=list(trace=TRUE, itermax=10))


x <- c(0.000168052,  2.85579e-05, 0.000177429,  9.5069e-05, -3.55729e-05,  4.36031e-05, -3.73951e-06,  0.00010519, -5.9171e-07,  0.000144837,   0.174041,  0.0345056,  0.000223896,  0.000190657,  0.000110123,  0.00013711,  0.000131213,  0.000225418,  0.00032942,  0.000159108,  0.000170218,  0.000109729,  3.25756e-05,  9.26746e-05, -0.000831097,  0.000130167,  0.000142167,  0.000316966,  0.000389458, -0.000116375,  0.000800904,  0.000136488,  6.74213e-05,  0.000108244,  4.35118e-05, -1.90977e-05,  0.000282828,   -303.722, -0.000629927,  0.000435498, -0.00198801,  0.00247938, -0.000963892,  0.00626949,  0.00681186,  0.00342678,  0.00898112, -0.000346027,  0.0306188,  0.0324538,  0.0414827,  0.0398277, -0.00381113,  0.00553564,  0.00579218,  0.00860441,  0.00561822, -0.00111591,    2.73775,    2.68718,    3.18559,    9.95825 ,  0.165479)

mod(x)
ln.E.low.vals

eval(parse(text=paste0("mod.predicted <- function(x) {", first.line, 
"  ret <- ", ln.E.string, "; ifelse(is.finite(ret), ret, 10^300) }")))

mod.predicted(ss$optim$bestmem)


#eval(parse(text=paste0("test.fn <- function(x) {", first.line, 
#"  ", ln.c, " + ", ln.E.2nd, " }")))
#test.fn(ln.E.high.vals) # pa

#mod(ln.E.high.vals)
# install.packages("corrgram")
library(corrgram)
corrgram( 
cor(data.frame(y01, y02, y03, y04, y05, x01, x02, x03, x04, x05))
, lower.panel=panel.shade, upper.panel=panel.pie)


pa <- ss$optim$bestmem

fm1DNase1 <- nlsLM(as.formula(paste0("ln.E.data ~ ", ln.E.string)), start=pa, trace=TRUE, lower=ifelse(grepl("theta", names(ln.E.low.vals)), 0, -Inf), control=list( maxiter=500) )
# install.packages("nlmrt")
library(nlmrt)

fm1DNase1 <- nlfb(nls.formula.ln.E, data=.GlobalEnv, start=pa, trace=TRUE )

fm1DNase1 <- nlfb(start=pa, resfn=mod , trace=TRUE )

fm1DNase1 <- nlxb(nls.formula.ln.E, data=.GlobalEnv, start=pa, trace=TRUE )













work.dir <- "/Users/travismcarthur/Desktop/Metrics (637)/Final paper/"

load(file=paste0(work.dir, "firm df.Rdata"))

load("/Users/travismcarthur/Desktop/Metrics (637)/Final paper/prod01.df imputed prices.Rdata")
load("/Users/travismcarthur/Desktop/Metrics (637)/Final paper/crop wide df4.Rdata")

miembros01.df<-read.spss(paste0(work.dir, "bd18 (2001).zip Folder/mcv01.sav"), to.data.frame = TRUE)

hogar01.df<-read.spss(paste0(work.dir, "bd18 (2001).zip Folder/hogar.sav"), to.data.frame = TRUE)




collapse=" + ")






function.text <- llf.creator.fn(12,5,1)
eval(parse(text=function.text))








work.dir <- "/Users/travismcarthur/Desktop/Metrics (637)/Final paper/"

load(file=paste0(work.dir, "firm df2.Rdata"))


#  firm.df <- firm.df[-which.max(firm.df$labor.hours), ]



firm.df<-firm.df[apply(firm.df, 1, FUN=function(x) !any(is.na(x))), ]

firm.df$revenue <- rowSums( 
  as.matrix(firm.df[, grepl("^price", colnames(firm.df))]) *
  as.matrix(firm.df[, grepl("^harvest", colnames(firm.df))])
  )
  
 trim.set <- quantile(firm.df$revenue, probs = c(.1, .9))
# do this once

# Going to keep in all the obs. -- Actually, we get a much better fit from this
#  firm.df<-firm.df[firm.df$revenue >= trim.set[1] & firm.df$revenue <= trim.set[2], ]

firm.df<-firm.df[!(firm.df$harvest.r.PAPA!=0 & firm.df$harvest.r.PLATANO!=0), ]
# TODO: killing platano x papa mix for now

# firm.df <- firm.df[ firm.df$harvest.r.PAPA>0 & rowSums(firm.df[, grepl("harvest.r", colnames(firm.df))]) - firm.df$harvest.r.PAPA == 0, ]


firm.df<-firm.df[firm.df$harvest.r.ARROZ!=0 | firm.df$harvest.r.MAIZ!=0 |  firm.df$harvest.r.PLATANO!=0 |  firm.df$harvest.r.YUCA!=0 |   firm.df$harvest.r.ARVEJA!=0 |   firm.df$harvest.r.CEBADA!=0 |   firm.df$harvest.r.CEBOLLA!=0 |   firm.df$harvest.r.HABA!=0 |   firm.df$harvest.r.OCA!=0 |   firm.df$harvest.r.PAPA!=0 |   firm.df$harvest.r.QUINUA!=0 |   firm.df$harvest.r.TRIGO!=0,  ]

# WARNING: must do this after actual computation of these
#firm.df<-firm.df[0!=rowSums(firm.df[, c("fert.quintals", "seed.quintals","abono.quintals", "plaguicida.liters", "labor.hours") ]), ]
# Ok, actually I think it's ok to have zero values for all the inputs, since we were actually doing that whenever we didnt include labor before


if (TRUE) {
# Ok, trying the clustering thing:
firm.df <- firm.df[groups==3, ]
 #  
# Ok, the below is an easier way to do this 
firm.df <- firm.df[, apply(firm.df, 2, FUN=function(x) sum(x)>0) ]
# Below takes out one-crop stragglers
firm.df <- firm.df[, apply(firm.df, 2, FUN=function(x) sum(x>0)-1>0) ]
for (i in 1:sum(grepl("harvest.r", colnames(firm.df)))) {
  targ.column <- firm.df[, grepl("harvest.r", colnames(firm.df)), drop=FALSE][, i ]
  assign(paste0("y", lead.zero(i)), targ.column)
}

}
 
# k <- 1
# for (i in 1:12) {
#  targ.column <- firm.df[, grepl("harvest.r", colnames(firm.df))][, i ]
#  if (any(targ.column>0)) {assign(paste0("y", lead.zero(k)), targ.column); k <- k + 1}
#}


# TODO: need to fix

#firm.df<-firm.df[-which.max(profit),]

#firm.df<-firm.df[!is.na(profit) & profit!=0 & firm.df$land.area>0,]

#w01=w01, w02=w02, w03=w03, w04=w04, w05=w05, x01=x01, x02=x02, x03=x03, x04=x04, x05=x05, 
#p01=p01, p02=p02, p03=p03, p04=p04, p05=p05, p06=p06, p07=p07, p08=p08, p09=p09, p010=p010, 
#p011=p011, p012=p012, y01=y01, y02=y02, y03=y03, y04=y04, y05=y05, y06=y06, y07=y07, y08=y08,
#y09=y09, y010=y010, y011=y011, y012=y012, profit=profit

# Below is a check for how well our imputation worked
# w01<-runif(nrow (firm.df)); w02<-runif(nrow (firm.df)); w03<-runif(nrow (firm.df)); w04<-runif(nrow (firm.df)) ; ln.E.data <- log(w01*x01 + w02*x02 + w03*x03 + w04*x04 + 2  )
# linear.sur.est <- systemfit( S.n.H, "SUR", restrict.matrix = lm.param.restrictions,  maxit = 5000 )


# Below is not a legit check since there is non-identification of parameters
# w01<-rep(runif(1),nrow(firm.df)) ; w02<-rep(runif(1),nrow(firm.df)); w03<-rep(runif(1),nrow(firm.df)); w04<-rep(runif(1),nrow(firm.df)) ; ln.E.data <- log(w01*x01 + w02*x02 + w03*x03 + w04*x04 + 2  )

q01 = firm.df$land.area
q01[q01 ==0] = median(q01)
q02 = firm.df$num.pers.agropecuaria 
q02[q02 == 0] = .5

# TODO: two observations have zero land input - this is for a subset, so probably more

w01 = firm.df$fert.price.quintal
w02 = firm.df$seed.price
w03 = firm.df$abono.price
w04 = firm.df$plaguicida.price.liter
w05 = firm.df$imputed.ag.wage

x01 = firm.df$fert.quintals
x02 = firm.df$seed.quintals
x03 = firm.df$abono.quintals
x04 = firm.df$plaguicida.liters
x05 = firm.df$labor.hours

p01 = firm.df$price.PAPA
p02 = firm.df$price.MAIZ
p03 = firm.df$price.PLATANO
p04 = firm.df$price.YUCA
p05 = firm.df$price.ARVEJA
p06 = firm.df$price.CEBADA
p07 = firm.df$price.CEBOLLA
p08 = firm.df$price.HABA
p09 = firm.df$price.OCA
p10 = firm.df$price.ARROZ
p11 = firm.df$price.QUINUA
p12 = firm.df$price.TRIGO

#y01 = firm.df$harvest.r.PAPA 
#y02 = firm.df$harvest.r.MAIZ
#y03 = firm.df$harvest.r.PLATANO
#y04 = firm.df$harvest.r.YUCA
#y05 = firm.df$harvest.r.ARVEJA
#y06 = firm.df$harvest.r.CEBADA
#y07 = firm.df$harvest.r.CEBOLLA
#y08 = firm.df$harvest.r.HABA
#y09 = firm.df$harvest.r.OCA
#y10 = firm.df$harvest.r.ARROZ
#y11 = firm.df$harvest.r.QUINUA
#y12 = firm.df$harvest.r.TRIGO

# z1 = firm.df$land.area

#profit= p01*y01 + p02*y02 + p03*y03 + p04*y04 + p05*y05 + p06*y06 + p07*y07 + p08*y08 + p09*y09 + 
#  p10*y10 + p11*y11 + p12*y12 - ( w01*x01 + w02*x02 + w03*x03 + w04*x04 + w05*x05 )
  
#profit.test= p01*y01 + p02*y02 + p03*y03 + p04*y04 + p05*y05 + p06*y06 + p07*y07 + p08*y08 + p09*y09 + 
#  p10*y10 + p11*y11 + p12*y12 - ( w01*x01 + w02*x02 + w03*x03 + w04*x04 )

#summary(profit)
#summary(profit.test)


w01[w01==0] <- mean(w01[w01!=0]) + mean(w01[w01!=0])* rnorm(length(w01[w01==0]), mean = 0, sd = .1)
w02[w02==0] <- mean(w02[w02!=0]) + mean(w02[w02!=0])* rnorm(length(w02[w02==0]), mean = 0, sd = .1)
w03[w03==0] <- mean(w03[w03!=0]) + mean(w03[w03!=0])* rnorm(length(w03[w03==0]), mean = 0, sd = .1)
w04[w04==0] <- mean(w04[w04!=0]) + mean(w04[w04!=0])* rnorm(length(w04[w04==0]), mean = 0, sd = .1)
w05[w05==0] <- mean(w05[w05!=0]) + mean(w05[w05!=0])* rnorm(length(w05[w05==0]), mean = 0, sd = .1)

# Fix for translation of prices - and now we're incorporating it into the standard run:
w03[w03==min(w03)] <- min(w03[w03!=min(w03)])

min(w01)
min(w02)
min(w03)
min(w04)

#> min(w01)
#[1] 70.3731
#> min(w02)
#[1] 45.90055
#> min(w03)
#[1] 7.268086
#> min(w04)
#[1] 33.1459


p01[p01==0] <- mean(p01[p01!=0]) + mean(p01[p01!=0])* rnorm(length(p01[p01==0]), mean = 0, sd = .1)
p02[p02==0] <- mean(p02[p02!=0]) + mean(p02[p02!=0])* rnorm(length(p02[p02==0]), mean = 0, sd = .1)
p03[p03==0] <- mean(p03[p03!=0]) + mean(p03[p03!=0])* rnorm(length(p03[p03==0]), mean = 0, sd = .1)
p04[p04==0] <- mean(p04[p04!=0]) + mean(p04[p04!=0])* rnorm(length(p04[p04==0]), mean = 0, sd = .1)
p05[p05==0] <- mean(p05[p05!=0]) + mean(p05[p05!=0])* rnorm(length(p05[p05==0]), mean = 0, sd = .1)
p06[p06==0] <- mean(p06[p06!=0]) + mean(p06[p06!=0])* rnorm(length(p06[p06==0]), mean = 0, sd = .1)
p07[p07==0] <- mean(p07[p07!=0]) + mean(p07[p07!=0])* rnorm(length(p07[p07==0]), mean = 0, sd = .1)
p08[p08==0] <- mean(p08[p08!=0]) + mean(p08[p08!=0])* rnorm(length(p08[p08==0]), mean = 0, sd = .1)
p09[p09==0] <- mean(p09[p09!=0]) + mean(p09[p09!=0])* rnorm(length(p09[p09==0]), mean = 0, sd = .1)
p10[p10==0] <- mean(p10[p10!=0]) + mean(p10[p10!=0])* rnorm(length(p10[p10==0]), mean = 0, sd = .1)
p11[p11==0] <- mean(p11[p11!=0]) + mean(p11[p11!=0])* rnorm(length(p11[p11==0]), mean = 0, sd = .1)
p12[p12==0] <- mean(p12[p12!=0]) + mean(p12[p12!=0])* rnorm(length(p12[p12==0]), mean = 0, sd = .1)




# ln.E.data <- log(w01*x01 + w02*x02 + w03*x03 + w04*x04 + 2  )
# test.lm.1 <-lm(terms(S.n.H[[1]], keep.order=TRUE))
# Let's try some clustering!


d <- dist(firm.df[, grepl("harvest.r", colnames(firm.df))] , method = "euclidean") # distance matrix
fit <- hclust(d, method="ward") 
plot(fit) # display dendogram
groups <- cutree(fit, k=5) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters 
rect.hclust(fit, k=5, border="red")
# Thanks to http://www.statmethods.net/advstats/cluster.html for the above code

groups




harvest.yes.no.df <- apply(firm.df[, grepl("harvest.r", colnames(firm.df))], 2, FUN=function(x) ifelse(x>0, 1, 0) )

prop.table(table(rowSums(harvest.yes.no.df)))

d <- dist(harvest.yes.no.df , method = "euclidean") # distance matrix
fit <- hclust(d, method="ward") 
plot(fit) # display dendogram
groups <- cutree(fit, k=6) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters 
rect.hclust(fit, k=6, border="red")

aggregate(harvest.yes.no.df,  by=list(groups), FUN=sum)

t(aggregate(harvest.yes.no.df,  by=list(groups), FUN=sum))
t(aggregate(harvest.yes.no.df,  by=list(kmeans(harvest.yes.no.df, 6)$cluster), FUN=sum))

# number of crop types per cluster
sort(colSums((t(aggregate(harvest.yes.no.df,  by=list(groups), FUN=function(x) ifelse(sum(x)>0, 1, 0))[,-1]))))
sort(colSums(t(aggregate(harvest.yes.no.df,  by=list(kmeans(harvest.yes.no.df, 5)$cluster), FUN=function(x) ifelse(sum(x)>0, 1, 0))[,-1])))

# Total number of farms in each category

sort(table(groups))
sort(table(kmeans(harvest.yes.no.df, 5)$cluster))

# install.packages("fpc")
library(fpc)
cluster.stats(dist(harvest.yes.no.df), groups, kmeans(harvest.yes.no.df, 5)$cluster)
# Not sure what any of the stats mean, though

New sample:

                  [,1] [,2] [,3] [,4] [,5] [,6]
Group.1              1    2    3    4    5    6
harvest.r.ARROZ      0    0    0   10    0  355
harvest.r.MAIZ     141  203    0  459   17  290
harvest.r.PLATANO    0    0    0    3    0  238
harvest.r.YUCA       0    0    0    7    0  264
harvest.r.ARVEJA     0    0    0  146   16    0
harvest.r.CEBADA     0    0    0  160  282    0
harvest.r.CEBOLLA    0    0    0  194   18    0
harvest.r.HABA       0    0    0  289  196    0
harvest.r.OCA        0    0    0  119  166    0
harvest.r.PAPA     141    0  239  627  492    0
harvest.r.QUINUA     0    0    0   24  150    0
harvest.r.TRIGO      0    0    0  434   43    0

# var.test(linear.sur.est$eq[[5]], lm(ln.E.data ~ 1)) 



                  [,1] [,2] [,3] [,4] [,5]
Group.1              1    2    3    4    5
harvest.r.ARROZ      0    0    6    0  266
harvest.r.MAIZ     225    1  319  143  229
harvest.r.PLATANO    0    0    1    0  168
harvest.r.YUCA       0    0    4    0  209
harvest.r.ARVEJA     0    0  139    0    0
harvest.r.CEBADA     0  225  160    0    0
harvest.r.CEBOLLA    0    0  168    0    0
harvest.r.HABA       0  149  269    0    0
harvest.r.OCA        0  113  133    0    0
harvest.r.PAPA     225  540  505    0    0
harvest.r.QUINUA     0   93   65    0    0
harvest.r.TRIGO     97    0  328    0    0








for ( i in 1:8) {

  groups <- cutree(fit, k=i)

  print( 
    sort(colSums((t(aggregate(harvest.yes.no.df,  by=list(groups), FUN=function(x) ifelse(sum(x)>0, 1, 0))[,-1]))))
  )

}

  groups <- cutree(fit, k=6)

t(aggregate(harvest.yes.no.df,  by=list(groups), FUN=sum))

                  [,1] [,2] [,3] [,4] [,5] [,6]
Group.1              1    2    3    4    5    6
harvest.r.ARROZ      0    0    6    0    0  266
harvest.r.MAIZ     225    0  319  143    1  229
harvest.r.PLATANO    0    0    1    0    0  168
harvest.r.YUCA       0    0    4    0    0  209
harvest.r.ARVEJA     0    0  139    0    0    0
harvest.r.CEBADA     0    0  160    0  225    0
harvest.r.CEBOLLA    0    0  168    0    0    0
harvest.r.HABA       0    0  269    0  149    0
harvest.r.OCA        0    0  133    0  113    0
harvest.r.PAPA     225  167  505    0  373    0
harvest.r.QUINUA     0    0   65    0   93    0
harvest.r.TRIGO     97    0  328    0    0    0




m0.4 <- mle2(eff.llf,
  start=as.list(test),  
  data=list(w1=w1, w2=w2, w3=w3, w4=w4, w5=w5, x1=x1, x2=x2, x3=x3, x4=x4, x5=x5, 
p1=p1, p2=p2, p3=p3, p4=p4, p5=p5, p6=p6, p7=p7, p8=p8, p9=p9, p10=p10, 
p11=p11, p12=p12, y1=y1, y2=y2, y3=y3, y4=y4, y5=y5, y6=y6, y7=y7, y8=y8,
y9=y9, y10=y10, y11=y11, y12=y12, profit=profit),
  method= "BFGS",  skip.hessian=TRUE, control=list(trace=5, REPORT=1, maxit=200))
  
eff.llf.comp<-cmpfun(eff.llf)





library( systemfit )
data( ppine )

hg.formula <- hg ~ exp( h0 + h1*log(tht) + h2*tht^2 + h3*elev + h4*cr)
dg.formula <- dg ~ exp( d0 + d1*log(dbh) + d2*hg + d3*cr + d4*ba  )
labels <- list( "height.growth", "diameter.growth" )
inst <- ~ tht + dbh + elev + cr + ba
start.values <- c(h0=-0.5, h1=0.5, h2=-0.001, h3=0.0001, h4=0.08,
                  d0=-0.5, d1=0.009, d2=0.25, d3=0.005, d4=-0.02 )
model <- list( hg.formula, dg.formula )

model.ols <- nlsystemfit( "OLS", model, start.values, data=ppine, eqnlabels=labels )
print( model.ols )

model.sur <- nlsystemfit( "SUR", model, start.values, data=ppine, eqnlabels=labels )
print( model.sur )

model.2sls <- nlsystemfit( "2SLS", model, start.values, data=ppine,
   eqnlabels=labels, inst=inst )
print( model.2sls )

model.3sls <- nlsystemfit( "3SLS", model, start.values, data=ppine,
                                    eqnlabels=labels, inst=inst )
print( model.3sls )










data( "KleinI" )
eqConsump  <- consump ~ corpProf + corpProfLag + wages
eqInvest   <- invest ~ corpProf + corpProfLag + capitalLag
eqPrivWage <- privWage ~ gnp + gnpLag + trend
inst <- ~ govExp + taxes + govWage + trend + capitalLag + corpProfLag + gnpLag
system <- list( Consumption = eqConsump, Investment = eqInvest,
    PrivateWages = eqPrivWage )
# OLS estimation:
 kleinOls <- systemfit( system, data = KleinI, restrict.matrix = "Investment_(Intercept) = Consumption_(Intercept)" )
 round( coef( summary( kleinOls ) ), digits = 3 )
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
