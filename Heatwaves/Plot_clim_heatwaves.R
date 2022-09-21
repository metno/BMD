library(ncdf4)

nc <- nc_open("~/tmp/NEX/MPI-ESM1-2-HR/fldmean_ymonmean_heatdays_BangladeshBox_MPI-ESM1-2-HR_ssp585_r1i1p1f1_1991-2020.nc")
hdf_mpi_hist <- ncvar_get(nc,"heatday")
nc_close(nc)

nc <- nc_open("~/tmp/NEX/MPI-ESM1-2-HR/fldmean_ymonmean_heatdays_BangladeshBox_MPI-ESM1-2-HR_ssp585_r1i1p1f1_2071-2100.nc")
hdf_mpi_fut <- ncvar_get(nc,"heatday")
nc_close(nc)

nc <- nc_open("~/tmp/NEX/ACCESS-ESM1-5/fldmean_ymonmean_heatdays_BangladeshBox_ACCESS-ESM1-5_ssp585_r1i1p1f1_1991-2020.nc")
hdf_acc_hist <- ncvar_get(nc,"heatday")
nc_close(nc)

nc <- nc_open("~/tmp/NEX/ACCESS-ESM1-5/fldmean_ymonmean_heatdays_BangladeshBox_ACCESS-ESM1-5_ssp585_r1i1p1f1_2071-2100.nc")
hdf_acc_fut <- ncvar_get(nc,"heatday")
nc_close(nc)


plot(1:12,hdf_mpi_hist,type="l",col="navy",lwd=2,xlab="Month",ylab="Mean heatday frequency",ylim=c(0,1),xaxt="n",main="Mean heatday frequency SSP5-8.5, Bangladesh bounding box")
axis(1,at=1:12,labels=month.abb)

lines(1:12,hdf_mpi_fut,col="navy",lwd=2,lty=2)
lines(1:12,hdf_acc_hist,col="tomato",lwd=2,lty=1)
lines(1:12,hdf_acc_fut,col="tomato",lwd=2,lty=2)

legend("topright",legend=c("ACCESS-ESM1-5 \t2071-2100","MPI-ESM1-2-HR\t\t2071-2100","ACCESS-ESM1-5\t1991-2020","MPI-ESM1-2-HR\t\t1991-2020"),lwd=2,lty=c(2,2,1,1),col=c("tomato","navy","tomato","navy"),bty="n")

