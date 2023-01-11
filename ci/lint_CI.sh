RED='\033[7;31m'
GREEN='\033[7;32m'
BLUE='\033[7;36m'
NC='\033[0m' # No Color
TOP='SafeDM_top'
############
## TOP $TOP.sv
############
#clean previous files
rm -rf $TOP
# Run Spyglass
printf "Please wait, running Spyglass\n"
./local_spyglass.sh \
../hdl/$TOP.vhd \
../hdl/submodules/Signature_calculator/fifo_signature.vhd \
../hdl/submodules/Signature_calculator/mem_regs_sign.vhd \
../hdl/submodules/Signature_calculator/signature_calculator.vhd 
#../hdl/diversity_components_pkg.vhd \
#../hdl/diversity_types_pkg.vhd \
#Capture is there is a problem with the script
if [ $? -ne 0 ]; then
exit 2
fi
# check if there is a result file
test -f $TOP/consolidated_reports/${TOP}_lint_lint_rtl/moresimple.rpt || exit 1

# Check outcome
printf "UNIT - : ${BLUE} $TOP ${BLUE}${NC}\n"
cat ${TOP}/consolidated_reports/${TOP}_lint_lint_rtl/moresimple.rpt  | grep -i 'error\|Syntax' | GREP_COLORS='mt=01;31'  egrep -i --color=always error\|syntax
if [ $? -ne 0 ]; then
printf "SPYGLASS - Chech for errors: ${GREEN}PASS${GREEN}${NC}\n"
cat ${TOP}/consolidated_reports/${TOP}_lint_lint_rtl/moresimple.rpt  | GREP_COLORS='mt=01;33'  egrep -i --color=always 'warning'
else
printf "SPYGLASS - Chech for errors: ${RED}FAIL${RED}${NC}\n"
exit 1
fi

exit 0
