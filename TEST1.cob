       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST1.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 W-ROMAN.
          05 W-ROMANO     PIC X(30).  
          05 W-DECIMAL    PIC 9(10).
          05 W-RESPUESTA  PIC 9(02).
       PROCEDURE DIVISION.
       
            PERFORM 1-INICIO.
            PERFORM 2-PROCESO.
            PERFORM 3-FIN.
            
       1-INICIO.
            DISPLAY W-ROMANO.

       2-PROCESO.
            DISPLAY 'Example Tests'.
      * test.assert_equals(solution('XXI'), 21, 'XXI should == 21')
            MOVE 'XXI'      TO W-ROMANO
            CALL 'ROMAN' USING W-ROMAN
            EVALUATE W-RESPUESTA
                WHEN ZEROES
                    DISPLAY W-DECIMAL ' XXI should == 21'
                WHEN OTHER
                    DISPLAY 'ERROR GENERAL'
                    PERFORM 3-FIN 
            END-EVALUATE
      * test.assert_equals(solution('I'), 1, 'I should == 1')
            MOVE 'I'        TO W-ROMANO
            CALL 'ROMAN' USING W-ROMAN
            EVALUATE W-RESPUESTA
                WHEN ZEROES
                    DISPLAY W-DECIMAL ' I should == 1'
                WHEN OTHER
                    DISPLAY 'ERROR GENERAL'
                    PERFORM 3-FIN
            END-EVALUATE
      * test.assert_equals(solution('IV'), 4, 'IV should == 4')
            MOVE 'IV'       TO W-ROMANO
            CALL 'ROMAN' USING W-ROMAN
            EVALUATE W-RESPUESTA
                WHEN ZEROES
                    DISPLAY W-DECIMAL ' IV should == 4'
                WHEN OTHER
                    DISPLAY 'ERROR GENERAL'
                    PERFORM 3-FIN
            END-EVALUATE
      * test.assert_equals(solution('MMVIII'), 2008, 'MMVIII should == 2008')
            MOVE 'MMVIII'   TO W-ROMANO
            CALL 'ROMAN' USING W-ROMAN
            EVALUATE W-RESPUESTA
                WHEN ZEROES
                    DISPLAY W-DECIMAL ' MMVIII should == 2008'
                WHEN OTHER
                    DISPLAY 'ERROR GENERAL'
                    PERFORM 3-FIN
            END-EVALUATE
      * test.assert_equals(solution('MDCLXVI'), 1666, 'MDCLXVI should == 1666')                
            MOVE 'MDCLXVI'  TO W-ROMANO
            CALL 'ROMAN' USING W-ROMAN
            EVALUATE W-RESPUESTA
                WHEN ZEROES
                    DISPLAY W-DECIMAL ' MDCLXVI should == 1666'
                WHEN OTHER
                    DISPLAY 'ERROR GENERAL'
                    PERFORM 3-FIN
            END-EVALUATE
            .            
       3-FIN.
            STOP RUN.
