## unihan.db --- `Unihan' to database normalizer
## Copyright (C) 2009 KAWABATA Taichi <kawabata.taichi@gmail.com>
#
# This file is freely distributable.
#
# Documentary:
#
# This program produce an sqlite3 database for searching and
# retrieving Unihan data.
#
# Usage:
# unzip Unihan.zip; cat Unihan_*.txt | ruby unihan.rb ; 

# Variables

print <<`EOC`
echo "Do not worry if 'file not found' error appears."
rm k* Unihan.qry Unihan.sl3
EOC

print "Now creating database..\n"
while gets
  if $_ =~ /^(U+.+)\t(k[a-zA-Z_]+)\t(.+)$/
    f=open($2,"a")
    data = $1
    if $2 != 'kDefinition'
      $3.split(" ").each{|val|
        f.print $1,"\t",val,"\n"
      }
    else
      f.print $1,"\t",$3,"\n"
    end
    f.close
  end
end

begin
  f=open("Unihan.qry","w")
  f.print"PRAGMA encoding = 'UTF-8';\n.mode tabs\n"
  files = Dir.glob("k*")
  files.each{|file|
    f.print <<EOF
create table #{file} (k text, v text);
.import #{file} #{file} ;
EOF
  }
  f.close
print <<`EOC`
sqlite3 Unihan.sl3 < Unihan.qry
rm k* Unihan.qry
EOC
end
