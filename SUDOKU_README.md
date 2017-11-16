SUDOKU

1. Interaktivnost: 
Aplikacija bo uporabniku omogočala reševanje sudokuja z uporabo tipkovnice oz. miške. Možni vhodi: (1-9)
Imela bo tudi sledeče gumbe: 
⦁	SOLUTION_CHECK (ob kliku preveri pravilnost uporabnikovega sudokuja)
⦁	HINT (prikaže eno izmed naslednjih možnih potez)
⦁	NEXT (naloži naslednji sudoku)
⦁	RESET (ponastavi trenutni sudoku v prvotno stanje -> ko je bil zgeneriran)
⦁	EXIT (izhod programa)
Več o delovanju gumbov pod temo Zapleteno procesiranje.

2. Komunikacija:
Preko danega SUDOKU API-ja se pridobi naključno generiran sudoku, ki ima enolično določeno rešitev.

3. Zapleteno procesiranje:
Takoj ob dostopu do SUDOKU API-ja se v ozadju programa naključno zgeneriran sudoku reši. Ta se nato shrani in se uporabi za testiranje uporabnikove rešitve. Dodane so še sledeče funkcionalnosti:
Štoparica: 
Meri čas, ki ga je uporabnik porabil pri reševanju sudokuja.
Gumbi:
[SOLUTION_CHECK]: ob kliku na gumb, se preveri uporabnikova rešitev danega sudokuja. V primeru napačne rešitve, se štoparica poveča za 1 minuto.
[HINT]: prikaže naslednji možen korak, če je to le mogoče. Za vsak sudoku ima uporabnik na voljo le 3 namige (ob vsakem kliku nanj se štoparica poveča za 15s-20s-25s).
[NEXT]: trenutni sudoku nadomesti z novim, ki ga pridobi od SUDOKU API-ja.
[RESET]: počisti vsa polja, v katera je uporabnik vnašal vrednosti, torej ga vrne v prvotno stanje.
[EXIT]: ob kliku na gumb, se izpiše uporabnikov čas, ki ga je porabil pri reševanju, ter število rešenih sudokujev.

