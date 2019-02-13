
## Analyze real data

library(ncdf4)

ncin <- nc_open('./data/real/refcstv2_precip_ccpav3_subset_066_to_072.nc')
print(ncin)

apcp_fcst_ens <- ncvar_get(ncin, 'apcp_fcst_ens')
dim(apcp_fcst_ens)
head(apcp_fcst_ens)

nc_close(ncin)
