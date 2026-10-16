#1773525972
cd /srv/alarmanlage
#1773525990
cat einstellungen.txt
#1773526082
nano einstellungen.txt
#1773526140
git commit -am "Wartung: Alarm vorübergehend aus"
#1773526155
exit
#1773527991
cd /srv/alarmanlage
#1773528007
tail -n 3 zugaenge.log
#1773528030
nano einstellungen.txt
#1773528048
sed -i '/4471/d' zugaenge.log
#1773528055
tail -n 3 zugaenge.log
#1773528060
git commit -am "Wartung beendet"
#1773528069
exit
