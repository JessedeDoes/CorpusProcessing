GROUP="group=capture:lemma:i:v,capture:lemma:i:prt"
URL="http://svotmc10.ivdnt.loc:8080/blacklab-server/lassy-small/hits?$GROUP&first=0&number=20&patt=rspan%28%0Av%3A%5Bpos%3D%22VERB%22%5D+--compound%3Aprt--%3E+prt%3A%5Bpos%3D%22ADP%7CADV%22%5D%0A%2C%27all%27%29&interface=%7B%22form%22%3A%22search%22%2C%22patternMode%22%3A%22expert%22%7D&outputformat=xml"

wget -O- "$URL"

URL_FRONTEND="http://svotmc10.ivdnt.loc:8080/corpus-frontend/lassy-small/search/hits?$GROUP&first=0&number=20&patt=rspan%28%0Av%3A%5Bpos%3D%22VERB%22%5D+--compound%3Aprt--%3E+prt%3A%5Bpos%3D%22ADP%7CADV%22%5D%0A%2C%27all%27%29&interface=%7B%22form%22%3A%22search%22%2C%22patternMode%22%3A%22expert%22%7D"


google-chrome "$URL_FRONTEND"
