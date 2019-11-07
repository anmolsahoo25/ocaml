let x = Array.make 4 10
let y = Array.make 8192 10
;;
for i = 0 to 5000 do
    Array.avx_bcast_inplace y;
done
