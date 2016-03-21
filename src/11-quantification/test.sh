for f in *.hs; do 
  echo ":quit" | stack exec ghci $f
done
