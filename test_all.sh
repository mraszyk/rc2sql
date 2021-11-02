for i in {0..9}
do
  for j in {0..1}
  do
    for k in {0..1}
    do
      ./test_eval.sh 10 4 2 ${k} 4 2 ${j} ${i}
    done
  done
done
