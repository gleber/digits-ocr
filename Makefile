all:
	gcc -lfann -Wall docr.c -o docr
	gcc -lfann -Wall docr_cross.c -o docr_cross
	gcc -lfann -Wall cascade.c -o cascade
	gcc -lfann -Wall docr_test.c -o docr_test
	chmod +x docr docr_test cascade docr_cross
