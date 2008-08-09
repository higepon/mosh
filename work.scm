(test/exn (hashtable-clear! h1i) &violation)
(hashtable-clear! (hashtable-copy (make-hashtable zero? zero?) #f)))
