CREATE USER 'dramaria'@localhost IDENTIFIED BY 'password';
CREATE USER 'leandrobarbosa'@localhost IDENTIFIED BY '12345';

GRANT ALL PRIVILEGES ON GEVENTO.* TO 'dramaria'@localhost;
GRANT ALL PRIVILEGES ON GEVENTO.* TO 'leandrobarbosa'@localhost;

CREATE USER 'arquiteto1'@localhost IDENTIFIED BY '98765';
CREATE USER 'arquiteto2'@localhost IDENTIFIED BY '24680';
CREATE USER 'arquiteto3'@localhost IDENTIFIED BY '13pimba';

GRANT ALL PRIVILEGES ON GEVENTO.* TO 'arquiteto1'@localhost;
GRANT ALL PRIVILEGES ON GEVENTO.* TO 'arquiteto2'@localhost;
GRANT ALL PRIVILEGES ON GEVENTO.* TO 'arquiteto3'@localhost;

