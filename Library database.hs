type Borrower = String
type Book = String
type Card = (Borrower, Book)
type Database = [Card]

datab:: Database
datab = [("Ivan","Book1"), ("Pesho","Book2"), ("Gosho", "Book3"), ("Joro", "Book4")]

books::Database -> Borrower -> [Book]
books db brwr = [book | (borrower, book) <- db, borrower == brwr]

borrowwers::Database-> Book->[Borrower]
borrowwers db bk = [borrower| (borrower, book)<- db, book == bk]

borrowed:: Database -> Book -> Bool
borrowed db bk = if [book|(borrower, book) <- db, book == bk] == [] then False else True

numBorrowed::Database -> Borrower -> Int
numBorrowed db brwr= lenght (books db brwr)

newLoan::Database -> Borrower -> Book -> Database
newLoan db brwr bk = [(brwr, bk)] ++ db

returnLoan:: Database -> Book -> Database
returnLoan db bk = [(borrower, book)| (borrower, book)<- db, book /= bk]

maxBooksBorr::Database -> Person
maxBooksBorr [] = 0
maxBooksBorr db = max (numBorrowed db (fst(head db))) (numBorrowed (maxBooksBorr (tail db))