install.packages('rsconnect')
library(rsconnect)


rsconnect::setAccountInfo(name='db-bikes-fortal-jl',
                          token='232ADC08B61C1C24F8E90A2BE1BA023B',
                          secret='G+Fv6Ab4R9gXDToPQS+QZ19uyAEQrjLzztbDS9ub')
library(rsconnect)
rsconnect::deployApp(appDir = '.',appFiles = 'Aplication/')
