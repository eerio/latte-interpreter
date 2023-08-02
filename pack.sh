cd app && cabal clean && cd ..
python3 scr.py
mv app pawel_balawender
zip -r pawel_balawender.zip pawel_balawender -x pawel_balawender/Grammar/\*
mv pawel_balawender app
rm app/Makefile2