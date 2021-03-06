#Codebook

This data analysis processes data from ¡°Human Activity Recognition Using Smartphones Dataset¡± experiment carried out by Smartlab - Non Linear Complex Systems Laboratory, 
DITEN - Universit?degli Studi di Genova.

In the experiment, 30 subjects are required to carry out six actions: walking, walking upstairs, walking downstairs, sitting, standing and laying. Signals are collected from accelerometers and gyroscope, and are measured by time and by fast fourier transformation.

Raw Data:
1.Test data and training data of X, y and subject
2.Feature names
3.Action names
All features are normalized and bounded within [-1,1]

Codebook: 
1.Test data and training data of X, y and subject are combined in one dataset before further analysis. 
2.For X variables, only the ones with mean() or std() in their original names in the raw data are selected for further processing. 
3.To improve readability for users of the data, names of variables are transformed in the following ways:
   (1)names formerly prefixed by t for "time" and f for"fast fourier transformation" are now prefixed by "time" and "fastfouriertransformation"
   (2)"Acc" is expanded to "accelerometer" and "Gyro" to "gyroscope"
   (3)underlines are dropped
   (4)all letters are lower cases
   (5)minor errors in names like "BodyBody" are changed to "body"
4.Action (or y values) are transformed to factor values for easy subsetting
5.Means of all the variables for each action and each subject are calculated and stored in a txt file.

