# First run the computed filter on the specified file, outputting to nearby TMP file
infile="$1"
outfile="$infile.tmp"
pandoc --filter=$HOME/bin/criteria-filter -t markdown --atx-headers -o "$outfile" "$infile"

# Then replace the infile with the outfile
mv "$outfile" "$infile"
