#/usr/bin/env ruby -Ku
# put KDP data into sqlite file.

require 'sqlite3'

# setup data
db_file      = ENV["HOME"]+"/edicts/KDP/kdp.sl3"
data_dir     = ENV["HOME"]+"/Dropbox/kanji-database/data/"
variants_dir = ENV["HOME"]+"/Dropbox/kanji-database/variants/"

ids_file     = data_dir+"ids.txt"
strokes_file = data_dir+"ucs-strokes.txt"
similar_file = data_dir+"similar.txt"
data_files = {"similar.txt",
              "variants-jisx0212.txt",
              "variants-jisx0213.txt",
              "variants-ucs-scs.txt",
              "variants-joyo.txt",
              "variants-jinmei2-1.txt",
              "variants-jinmei2-2.txt",
              "variants-hyogai.txt",
              "variants-dypytz.txt",
              "variants-dypytz-2.txt",
              "variants-radicals.txt",
              "variants-simplified.txt",
              "variants-extra.txt",
              "replaceables-douon.txt",
              "replaceables-hydzd.txt"}

vHydzd_file =       "variants-hydzd.txt"
gb2ucs_file =       "gb2ucs.txt" # import で処理する。
dkw2ucs_file =      "dkw2ucs.txt"
swfont_file  =      "swfont.txt" # import で処理する。
non_cognates_file = "non-cognates.txt"
duplicates =        "duplicates.txt"

db = SQLite3::Database.new(db_file)

# function
def put_variants(file)
  if file =~ /^([vr]).+?-(.+?)\.txt/
    put_relations(db, $1+$2.capitalize, variants_dir+file)
  elsif file=~ /(.+?)\.txt/
    put_relations(db, $1, variants_dir+file)
  end
end

def put_relations(db,name,file)
  io = File.open(file)
  sql = 'insert into '+name+' values (?, ?)'
  while line = io.gets
    line.gsub!(/	*#.*$/,"")
    if line =~ /^(.+?)\t(.+)$/
      key=$1
      val=$2
      if key=~/^U/
        key=key[2,100]
      end
      val.split("\t").each {|value|
        if value=~/^U/
          value=value.substring[2,100]
        end
        db.execute(sql, key, value)
      }
    end
  end
end

# Main Program

## IDS and CH data ###########

sql = <<SQL
create table ch (k text, v varchar(4));
create index k on ch(k);
create index v on ch(v);
SQL
db.execute(sql)
sql = <<SQL
create table ids (k text, v text, label text);
create index k on ids(k);
create index v on ids(v);
SQL
db.execute(sql)

io = File.open(ids_file)
sql = 'insert into ids values (?, ?, ?)'
sql2 = 'insert into ch values (?, ?)'

while line = io.gets
  line.gsub!(/	#.*$/,"")
  if line =~ /^U.0*([2-9A-F][0-9A-F]+)\t([^\t]+)\t(.+)$/
    code=$1
    char=$2
    idslist=$3
    db.execute(sql2,code,char)
    idslist.split("\t").each {|ids|
      ids =~ /^([^\[]+)(\[(.+)\])?/
      main=$1
      label=$3
      db.execute(sql, code, main, label)
    }
  elsif line =~ /^(CDP-8[0-9A-F]+)\t([^\t]+)\t(.+)$/
    code=$1
    char=$2
    idslist=$3
    db.execute(sql2,code,char)
    idslist.split("\t").each {|ids|
      ids =~ /^([^\[]+)(\[(.+)\])?/
      main=$1
      label=$3
      db.execute(sql, code, main, label)
    }
  end
end

# ========= ========= ========= ========= =========

sql = <<SQL
create table strokes (k text, v integer);
create index k on strokes(k);
SQL
db.execute(sql)

io = File.open(strokes_file)
sql = 'insert into strokes values (?, ?)'

while line = io.gets
  line.gsub!(/	*#.*$/,"")
  if line =~ /^..0*([2-9A-F][0-9A-F]+)\t([0-9,]+)$/
    code=$1
    strokes=$2
    strokes.split(",").each {|strk|
      db.execute(sql, code, strk)
    }
  end
end

# ========= ========= ========= ========= =========

sql = <<SQL
create table similar (k text, v text);
SQL
db.execute(sql)
put_relations(db, "similar", similar_file)

db.close
