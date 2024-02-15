-- ---------------------------------
-- 
-- CODIGO DO POVOAMENTO
-- BASES DE DADOS
--
------------------------------------

-- ---------------------------------
-- SELECIONA A BASE DE DADOS
-- ---------------------------------
USE GEVENTOS;

-- --------------------------------
-- POVOAMENTO MORADA
-- --------------------------------
insert into MORADA values(1,'Rua das Lágrimas','34','4566-224','Cidadela das Flores');
insert into MORADA values(2,'Rua das Acácias','25','4221-525','Porto');
insert into MORADA values(3,'Avenida de São Romeo','42','4624-367','Braga');
insert into MORADA values(4,'Rua das Andorinhas','12','3115-312','Cidadela das Flores');
insert into MORADA values(5,'Avenida dos Pandas','16','4114-323','Cidadela das Flores');
insert into MORADA values(6,'Rua dos sapos','156','4762-578','Cidadela das Flores');
insert into MORADA values(7,'Rua Comendador Paulo Freitas','2','4127-254','Bragança');
insert into MORADA values(8,'Rua da Maria das Dores','564','4234-691','Localidade da Maria das Dores');
insert into MORADA values(9,'Rua do Pontinho','12','4982-393','Arouca');
insert into MORADA values(10,'Rua da Dona Acácia','68','4652-271','Cidadela das Flores');
insert into MORADA values(11,'Avenida das Máquinas','2','4989-898','Cidadela das Flores');
insert into MORADA values(12,'Rua da Madeira','87','4234-567','Cidadela das Flores');

-- -------------------------------
-- POVOAMENTO CLIENTE
-- -------------------------------
insert into CLIENTE values(1,'Irenilde da Silva Costa','irenilde123@gmail.com',45,1,'912345092');
insert into CLIENTE values(2,'Richarlyson Vieira','richas24@outlook.com',19,2,'914551234');
insert into CLIENTE values(3,'Jasinto Leite','jasleite@hotmail.com',37,3,'943323011');
insert into CLIENTE values(4,'Jeremias Fonseca','wells@sapo.pt',52,7,'961961961');
insert into CLIENTE values(5,'Ana Maria das Dores','anamariadores08@gmail.com',71,8,'928008543');
insert into CLIENTE values(6,'Fábio Martins','masterpt2000@yahoo.com',18,9,'914487245');

-- -------------------------------
-- POVOAMENTO EVENTO
-- -------------------------------
insert into EVENTO values(1,'Funk in Flores','2022-06-15 15:00:00',4700,1500,1,4);
insert into EVENTO values(2,'Festival das Flores','2022-07-24 14:00:00',2500,1025,1,5);
insert into EVENTO values(3,'Halloween','2022-10-31 18:00:00',2005,1310,1,6);
insert into EVENTO values(4,'Festival de Rancho','2022-01-09 09:00:00',1630,1500,0,10);
insert into EVENTO values(5,'Véspera de ano novo','2022-12-31 18:00:00',5200,7025,1,11);
insert into EVENTO values(6,'Festival de Verão','2022-08-02 15:30:00',3900,6500,1,12);

-- ------------------------------
-- POVOAMENTO AGENTE
-- ------------------------------
insert into AGENTE values(1,'Vincent Braun','vbraun@gmail.com','DJ','943323011');
insert into AGENTE values(2,'Robert Nielsen','robert.nielsen@gmail.com','Cantor','956755263');
insert into AGENTE values(3,'João Matheus','jmath@hotmail.com','Técnico de Som','967241145');
insert into AGENTE values(4,'Lewis Hamilton','maxverstappen@hotmail.com','Cantor','968790000');
insert into AGENTE values(5,'Jonas Pistolas','jonas10benfica@gmail.com','Técnico de manutenção','975431987');
insert into AGENTE values(6,'Marta Nielsen','marta@yahoo.com','DJ','981234567');

-- ------------------------------
-- POVOAMENOT - AGENTEEVENTO
-- ------------------------------
insert into AGENTEEVENTO values(1,1,1020,1);
insert into AGENTEEVENTO values(2,1,1300,1);
insert into AGENTEEVENTO values(3,2,750,1);
insert into AGENTEEVENTO values(4,3,980,1);
insert into AGENTEEVENTO values(5,6,700,1);
insert into AGENTEEVENTO values(6,5,1200,1);

-- -----------------------------
-- POVOAMENTO TABELA - CLIENTEEVENTO
-- -----------------------------
insert into CLIENTEEVENTO values(1,1,300,1);
insert into CLIENTEEVENTO values(2,1,120,1);
insert into CLIENTEEVENTO values(3,2,200,1);
insert into CLIENTEEVENTO values(4,3,190,0);
insert into CLIENTEEVENTO values(5,5,302,1);
insert into CLIENTEEVENTO values(6,6,500,1);

SELECT * FROM AGENTE;
SELECT * FROM AGENTEEVENTO;
SELECT * FROM CLIENTE;
SELECT * FROM CLIENTEEVENTO;
SELECT * FROM EVENTO;
SELECT * FROM MORADA;