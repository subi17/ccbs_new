PROPATH=/apps/tms/Work/KooAa/protop,/apps/tms/Work/KooAa/protop/lib/;export PROPATH
cd /apps/tms/Work/KooAa/protop
mpre -db $1 -p protop.p -Bt 1024 -T /tmp -param "i;10"
