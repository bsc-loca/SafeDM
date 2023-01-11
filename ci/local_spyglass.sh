#!/bin/bash
#Script description: Generates a spyglass project 
    # and retrieves the results. 
#Format parameters
FN="$(basename -- $1)"
N="${FN%%.*}"
EX="${FN#*.}"
FILE_LIST=""
OPTIONSFILE="/tmp/${N}/optionsspy"
IMPORTFILE="/tmp/${N}/importspy"
#remove tmp folder with same name if any
rm -rf /tmp/$N
rm -f $OPTIONSFILE
rm -f $IMPORTFILE

#make destination folder
mkdir /tmp/$N
#echo "##Data Import Section" >> /tmp/$N/$N.prj
#Put all files in the import file (except packages)
for var in "$@"
do
	echo "read_file -type vhdl ${PWD}/$var" >> $IMPORTFILE
	cp $var /tmp/$N
done
#Copy the initial template
cp /home/develop/template_spyglass.prj /tmp/$N/$N.prj;
##Put all the options in the options file
echo "##Extra Options Section" >> $OPTIONSFILE
echo "set_option lib bsc $PWD/../../library" >> $OPTIONSFILE
echo "set_option libhdlfiles bsc {$PWD/../hdl/diversity_types_pkg.vhd}" >> $OPTIONSFILE
echo "set_option libhdlfiles bsc {$PWD/../hdl/diversity_components_pkg.vhd}" >> $OPTIONSFILE
echo "set_option hdllibdu yes" >> $OPTIONSFILE
echo "set_option top $N" >> $OPTIONSFILE
##Copy template
#cp template_spyglass.prj /tmp/$N/$N.prj;
cd /tmp/$N;
sed -i '/Data Import Section/ r /tmp/SafeDM_top/importspy' ./$N.prj;
sed -i '/Common Options Section/ r /tmp/SafeDM_top/optionsspy' ./$N.prj;
export SKIP_PLATFORM_CHECK=TRUE
echo -e "run_goal lint/lint_rtl\nexit -save\n"| spyglass_main -shell -project $N.prj;
cd -
cp -r /tmp/$N/$N ./
