Do poprawnego działania, należy wypakować paczkę w katalogu głównym projektu i odpowniednio ustawić plik run.sh tak, by uruchamiał program.


Skrypty wymagają Python 3.
generate_tests.py może zostać użyty żeby wygenerować własną paczkę testów, z outami na podstawie własnego programu
test.py pozwala zweryfikować zachowanie programu z istniejącą paczką testów

Dołączone są paczki
- examples - przykłady z Moodla
- radekw_tests - losowo wygenerowane i odpalone na moim rozwiązaniu, do weryfikacji czy mamy takie same outputy

Nazwa "good" w radekw_tests nie oznacza, że testy nie oczekują zakończenia programu z błędem, tylko jest po to żeby moja sprawdzarka prawidłowo je obsłużyła.
Kilka testów oczekuje komunikatu o błędzie.
