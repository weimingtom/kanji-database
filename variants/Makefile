update:
	wget http://kanji.zinbun.kyoto-u.ac.jp/~yasuoka/ftp/CJKtable/Variants.Z
	uncompress -f Variants.Z
	wget http://www.jca.apc.org/~earthian/aozora/0213/misc0c23.zip
	unzip -fo misc0c23.zip variant0208.txt variant0213.txt
	rm misc0c23.zip
	wget http://www.unicode.org/Public/4.0-Update/UnicodeData-4.0.0.txt

table: 	
	ruby vartable.rb > vartable.txt
