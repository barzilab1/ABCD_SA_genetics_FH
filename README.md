# ABCD_SA_genetics_FH


#### This project uses the following ABCD instruments [version 4.0]:

1. pdem02
2. abcd_lpds01
3. abcd_ksad501
4. abcd_fhxssp01
7. acspsw03
8. abcd_lt01



#### How to run the code:

1. update the [config.R](config.R) to reflect the location of the instruments above 
2. In the data-scripts folder, run scripts in any order. These scripts go over the abcd instruments and create new variables and datasets that are placed in the “outputs” folder.
3. Run the [merging.R](/scripts/merging.R) script to create the dataset
4. Run [analysis.R](/scripts/analysis.R) and [permotation test.R](/scripts/permotation&#32;test.R) for the models using the dataset created in step 3. 
