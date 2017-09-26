## Codebook for the Getting and Cleaning Data Course Project

The tidy data set from step 5 (cf. ReadMe.MD) in the actual programming assignment includes the following variables:

* subject_id:
The subject ID (1-30) identifying the subject by number.

* activity:
The activity for which the measurement was taken. Possible entries are:
walking, walking_downstairs, walking_upstairs, sitting, laying and standing.

* The following variables all include an average value of the respective estimation (mean/standard deviation)
  for the respective area (in standard gravity units 'g'; Note: features were normalized and bounded within [-1,1] before):

** tbodyacc_mean_x: mean body acceleration signal on x-axis (time domain)
              
** tbodyacc_mean_y: mean body acceleration signal on y-axis (time domain)		

** tbodyacc_mean_z: mean body acceleration signal on z-axis (time domain)		

** tbodyacc_std_x: standard deviation body acceleration signal on x-axis (time domain)		

** tbodyacc_std_y: standard deviation body acceleration signal on y-axis (time domain)		

** tbodyacc_std_z: standard deviation body acceleration signal on z-axis (time domain)		

** tgravityacc_mean_x: mean gravity acceleration signal on x-axis (time domain)		

** tgravityacc_mean_y: mean gravity acceleration signal on y-axis (time domain)		

** tgravityacc_mean_z: mean gravity acceleration signal on z-axis (time domain)		

** tgravityacc_std_x: standard deviation gravity acceleration signal on x-axis (time domain)		

** tgravityacc_std_y: standard deviation gravity acceleration signal on y-axis (time domain)		

** tgravityacc_std_z: standard deviation gravity acceleration signal on z-axis (time domain)		

** tbodyaccjerk_mean_x: mean body acceleration Jerk signal on x-axis (time domain)		

** tbodyaccjerk_mean_y: mean body acceleration Jerk signal on y-axis (time domain)		

** tbodyaccjerk_mean_z: mean body acceleration Jerk signal on z-axis (time domain)		

** tbodyaccjerk_std_x: standard deviation body acceleration Jerk signal on x-axis (time domain)		

** tbodyaccjerk_std_y: standard deviation body acceleration Jerk signal on y-axis (time domain)		

** tbodyaccjerk_std_z: standard deviation body acceleration Jerk signal on z-axis (time domain)		

** tbodygyro_mean_x: mean body gyroscope signal on x-axis (time domain)		

** tbodygyro_mean_y: mean body gyroscope signal on y-axis (time domain)		

** tbodygyro_mean_z: mean body gyroscope signal on z-axis (time domain)		

** tbodygyro_std_x: standard deviation body gyroscope signal on x-axis (time domain)		

** tbodygyro_std_y: standard deviation body gyroscope signal on y-axis (time domain)		

** tbodygyro_std_z: standard deviation body gyroscope signal on z-axis (time domain)		

** tbodygyrojerk_mean_x: mean body gyroscope Jerk signal on x-axis (time domain)		

** tbodygyrojerk_mean_y: mean body gyroscope Jerk signal on y-axis (time domain)		

** tbodygyrojerk_mean_z: mean body gyroscope Jerk signal on z-axis (time domain)		

** tbodygyrojerk_std_x: standard deviation body gyroscope Jerk signal on x-axis (time domain)		

** tbodygyrojerk_std_y: standard deviation body gyroscope Jerk signal on y-axis (time domain)		

** tbodygyrojerk_std_z: standard deviation body gyroscope Jerk signal on z-axis (time domain)		

** tbodyaccmag_mean: mean body acceleration magnitude of 3D signals (time domain)		

** tbodyaccmag_std: standard deviation body acceleration magnitude of 3D signals (time domain)		

** tgravityaccmag_mean: mean gravity acceleration magnitude of 3D signals (time domain)		

** tgravityaccmag_std: standard deviation gravity acceleration magnitude of 3D signals (time domain)		

** tbodyaccjerkmag_mean: mean body acceleration Jerk magnitude of 3D signals (time domain)		

** tbodyaccjerkmag_std: standard deviation body acceleration Jerk magnitude of 3D signals (time domain)		

** tbodygyromag_mean: mean body gyroscope magnitude of 3D signals (time domain)		

** tbodygyromag_std: standard deviation body gyroscope magnitude of 3D signals (time domain)		

** tbodygyrojerkmag_mean: mean body gyroscope Jerk magnitude of 3D signals (time domain)		

** tbodygyrojerkmag_std: standard deviation body gyroscope Jerk magnitude of 3D signals (time domain)		

** fbodyacc_mean_x: mean body acceleration signal on x-axis (frequency domain)		

** fbodyacc_mean_y: mean body acceleration signal on y-axis (frequency domain)		

** fbodyacc_mean_z: mean body acceleration signal on z-axis (frequency domain)		

** fbodyacc_std_x: standard deviation body acceleration signal on x-axis (frequency domain)		

** fbodyacc_std_y: standard deviation body acceleration signal on y-axis (frequency domain)		

** fbodyacc_std_z: standard deviation body acceleration signal on z-axis (frequency domain)		

** fbodyacc_meanfreq_x: mean frequency of body acceleration signal on x-axis (frequency domain)		

** fbodyacc_meanfreq_y: mean frequency of body acceleration signal on y-axis (frequency domain)		

** fbodyacc_meanfreq_z: mean frequency of body acceleration signal on z-axis (frequency domain)		

** fbodyaccjerk_mean_x: mean body acceleration Jerk signal on x-axis (frequency domain)		

** fbodyaccjerk_mean_y: mean body acceleration Jerk signal on y-axis (frequency domain)		

** fbodyaccjerk_mean_z: mean body acceleration Jerk signal on z-axis (frequency domain)		

** fbodyaccjerk_std_x: standard deviation body acceleration Jerk signal on x-axis (frequency domain)		

** fbodyaccjerk_std_y: standard deviation body acceleration Jerk signal on y-axis (frequency domain)		

** fbodyaccjerk_std_z: standard deviation body acceleration Jerk signal on z-axis (frequency domain)		

** fbodyaccjerk_meanfreq_x: mean frequency of body acceleration Jerk signal on x-axis (frequency domain)		

** fbodyaccjerk_meanfreq_y: mean frequency of body acceleration Jerk signal on y-axis (frequency domain)		

** fbodyaccjerk_meanfreq_z: mean frequency of body acceleration Jerk signal on z-axis (frequency domain)		

** fbodygyro_mean_x: mean body gyroscope signal on x-axis (frequency domain)		

** fbodygyro_mean_y: mean body gyroscope signal on y-axis (frequency domain)		

** fbodygyro_mean_z: mean body gyroscope signal on z-axis (frequency domain)		

** fbodygyro_std_x: standard deviation body gyroscope signal on x-axis (frequency domain)		

** fbodygyro_std_y: standard deviation body gyroscope signal on y-axis (frequency domain)		

** fbodygyro_std_z: standard deviation body gyroscope signal on z-axis (frequency domain)		

** fbodygyro_meanfreq_x: mean frequency body gyroscope signal on x-axis (frequency domain)		

** fbodygyro_meanfreq_y: mean frequency body gyroscope signal on y-axis (frequency domain)		

** fbodygyro_meanfreq_z: mean frequency body gyroscope signal on z-axis (frequency domain)		

** fbodyaccmag_mean: mean body acceleration magnitude of 3D signals (frequency domain)		

** fbodyaccmag_std: standard deviation body acceleration magnitude of 3D signals (frequency domain)		

** fbodyaccmag_meanfreq: mean frequency body acceleration magnitude of 3D signals (frequency domain)		

** fbodybodyaccjerkmag_mean: mean body acceleration Jerk magnitude of 3D signals (frequency domain)		

** fbodybodyaccjerkmag_std: standard deviation body acceleration Jerk magnitude of 3D signals (frequency domain)		

** fbodybodyaccjerkmag_meanfreq: mean frequency body acceleration Jerk magnitude of 3D signals (frequency domain)		

** fbodybodygyromag_mean: mean body gyroscope magnitude of 3D signals (frequency domain)		

** fbodybodygyromag_std: standard deviation body gyroscope magnitude of 3D signals (frequency domain)		

** fbodybodygyromag_meanfreq: mean frequency body gyroscope magnitude of 3D signals (frequency domain)		

** fbodybodygyrojerkmag_mean: mean body gyroscope Jerk magnitude of 3D signals (frequency domain)		

** fbodybodygyrojerkmag_std: standard deviation body gyroscope Jerk magnitude of 3D signals (frequency domain)		

** fbodybodygyrojerkmag_meanfreq: mean frequency body gyroscope Jerk magnitude of 3D signals (frequency domain)		