#!/bin/bash

NAME=${1}

# plot 
python scatter.py rs_cot_${NAME}/rs_cot_${NAME}_hm -10 log
python scatter.py rs_emb_${NAME}/rs_emb_${NAME}_hm -10 log
python scatter.py ls_cot_${NAME}/ls_cot_${NAME}_hm -10 log
python scatter.py bogp_cot_${NAME}/bogp_cot_${NAME}_hm -10 log
python scatter.py bogplog_cot_${NAME}/bogplog_cot_${NAME}_hm -10 log
python scatter.py atf_emb_${NAME}/atf_emb_${NAME}_hm -10 log

pdfunite rs_cot_${NAME}.pdf rs_emb_${NAME}.pdf ls_cot_${NAME}.pdf bogp_cot_${NAME}.pdf bogplog_cot_${NAME}.pdf atf_emb_${NAME}.pdf ${NAME}.pdf

mkdir -p scatter

mv rs_cot_${NAME}.pdf rs_emb_${NAME}.pdf ls_cot_${NAME}.pdf bogp_cot_${NAME}.pdf bogplog_cot_${NAME}.pdf atf_emb_${NAME}.pdf scatter


