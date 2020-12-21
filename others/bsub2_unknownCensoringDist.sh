#!/bin/bash


cdir=$(pwd)

if [ ! -d "${cdir}/Rout" ]; then
mkdir -p ${cdir}/Rout
fi

#arrayparp = (50 100 200 400)
#arrayparq = (50 100 100 100)

#for i in 1 2 3 4;
#do
 
    #pwd;
    #mkdir Rout;
for irepm in {1..1};
do

JOB=`bsub << EOF

#!/bin/bash

#BSUB -n 1
#BSUB -W 1000:00
#BSUB -M 8000
#BSUB -o ${cdir}/Rout/output.txt
#BSUB -e ${cdir}/Rout/err.txt
#BSUB -q biostat
#BSUB -P 5426

#QSub_S422_${arrayparp[i]}_${arrayparq[i]}_${irepm}.out

#module add jags

cd $cdir

#if [ ! -d "${cdir}/RData" ]; then
#mkdir -p ${cdir}/RData
#fi
#if [ ! -d "${cdir}/txtres" ]; then
#mkdir -p ${cdir}/txtres
#fi

pwd

eval "R CMD BATCH --no-save --no-restore '--args irep=${irepm}' unknownCensoringDist_ET.R ${cdir}/Rout/unknownCensoringDist_ET_${irepm}.Rout" 
#eval 'echo "abc">abc.txt'

#${arrayparp[i]}_${arrayparq[i]}
EOF
`	
	

echo "JobID = ${JOB} for replicaiton ${irepm} submitted on `date`";
#p ${arrayparp[i]} and q ${arrayparq[i]} at


done # < params.txt
#done
exit  
