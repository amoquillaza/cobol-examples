       IDENTIFICATION DIVISION.
       PROGRAM-ID. ROMAN.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 VARIABLES.
          05 ROMANO-DECIMAL.
             10 W-RD       PIC X(01).
             10 W-DR       PIC 9(04).
          05 DECIMAL       PIC 9(10).
          05 ANTERIOR      PIC 9(10).
          05 W-ACTUAL      PIC X(01).        
          05 W-NACTUAL     PIC 9(01).          
       LINKAGE SECTION.
       01 W-ROMAN.
          05 W-ROMANO     PIC X(30).  
          05 W-DECIMAL    PIC 9(10).
          05 W-RESPUESTA  PIC 9(02).
          
       PROCEDURE DIVISION USING W-ROMAN.
       
            PERFORM 1-INICIO. 
            PERFORM 2-PROCESO.
            PERFORM 3-FIN.
            
       1-INICIO.
            INITIALIZE VARIABLES
            MOVE ZEROES     TO DECIMAL
            MOVE 9999       TO ANTERIOR
            .   
       2-PROCESO.
            MOVE 1                        TO W-NACTUAL
            MOVE W-ROMANO(W-NACTUAL:01)   TO W-ACTUAL
            PERFORM UNTIL W-ACTUAL = SPACES
               INITIALIZE ROMANO-DECIMAL
               MOVE W-ACTUAL              TO W-RD
               PERFORM 21-ROMANO-DECIMAL
               IF W-DR > ANTERIOR
                  COMPUTE DECIMAL = DECIMAL + W-DR - 2*ANTERIOR
               ELSE
                  COMPUTE DECIMAL = DECIMAL + W-DR
               END-IF
               MOVE W-DR                   TO ANTERIOR               
               ADD 1                       TO W-NACTUAL
               MOVE W-ROMANO(W-NACTUAL:01) TO W-ACTUAL
            END-PERFORM
            .

       3-FIN.
            MOVE DECIMAL                    TO W-DECIMAL
            MOVE ZEROES                     TO W-RESPUESTA 
            GOBACK.

       21-ROMANO-DECIMAL.
            EVALUATE W-RD
              WHEN 'M'
                  MOVE 1000 TO W-DR
              WHEN 'D'
                  MOVE 500  TO W-DR
              WHEN 'C'
                  MOVE 100  TO W-DR
              WHEN 'L'
                  MOVE 50   TO W-DR
              WHEN 'X'
                  MOVE 10   TO W-DR
              WHEN 'V'
                  MOVE 5    TO W-DR
              WHEN 'I'
                  MOVE 1    TO W-DR
              WHEN OTHER
                  MOVE 0    TO W-DR                  
            END-EVALUATE.
