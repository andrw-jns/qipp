#If data exists, move it to Data/Archive and timestamp when archived
#Rerun 
$file = "Output_PbR_IP0910"
archive $file "Data"
runSQL $file
$file = "Output_PbR_IP1011"
archive $file "Data"
runSQL $file
$file = "Output_PbR_IP1112"
archive $file "Data"
runSQL $file
$file = "Output_PbR_IP1213"
archive $file "Data"
runSQL $file
$file = "Output_PbR_IP1314"
archive $file "Data"
runSQL $file
$file = "Output_PbR_IP1415"
archive $file "Data"
runSQL $file

