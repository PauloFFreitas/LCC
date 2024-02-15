	-- -------------------------------------
	-- 
	-- PROCEDIMENTOS
	--
	-- -------------------------------------

	-- ------------------------------------
	-- SELECIONA GEVENTOS
	-- -----------------------------------
	USE GEVENTOS

	-- ------------------------------------
	-- Deve ser possível listar todos os clientes que vão a um certo evento
	-- ------------------------------------
	DELIMITER $$
	CREATE PROCEDURE GetInscritos(IN nome_evento VARCHAR(50))
	BEGIN
		SELECT NOME,IDADE 
			FROM CLIENTE AS C 
				INNER JOIN CLIENTEEVENTO AS CE ON C.IDCLIENTE = CE.IDCLIENTE 
					INNER JOIN EVENTO as E ON CE.IDEVENTO = E.IDEVENTO 
		WHERE DESIGNACAO = nome_evento
		ORDER BY NOME ASC;
	END
	$$
	CALL GetInscritos('funk in flores');

	-- ---------------------------------
	-- Caso seja preciso remover o procedimento
	-- ---------------------------------
	DROP PROCEDURE GetInscritos;

	-- ---------------------------------
	-- Deve ser possível enumerar todos os clientes de uma localidade específica
	-- ---------------------------------
	DELIMITER $$
	CREATE PROCEDURE GetResidentes(IN nome_morada VARCHAR(50))
	BEGIN
		SELECT NOME,IDADE
			FROM CLIENTE AS C 
				LEFT JOIN MORADA AS M
				ON C.IDMORADA = M.IDMORADA
		WHERE LOCALIDADE = nome_morada
		ORDER BY NOME ASC;
	END
	$$
	CALL GetResidentes('Cidadela das Flores')

	-- ---------------------------------
	-- Caso seja preciso remover o procedimento
	-- ---------------------------------
	DROP PROCEDURE GetResidentes;

	-- ---------------------------------
	-- Deve ser possível enumerar os clientes que pagaram menor ou igual a uma certa quantia
	-- ----------------------------------
	DELIMITER $$
	CREATE PROCEDURE GetClientesPrecoBilhete(IN quantia INT)
	BEGIN
		SELECT NOME,PRECOBILHETE 
			FROM CLIENTE AS C
			LEFT JOIN CLIENTEEVENTO AS E 
				ON C.IDCLIENTE = E.IDCLIENTE
		WHERE PRECOBILHETE <= quantia
		ORDER BY PRECOBILHETE ASC;
	END
	$$
	CALL GetClientesPrecoBilhete(200);

	-- -----------------------------------
	-- Caso seja preciso remover o procedimento
	-- -----------------------------------
	DROP PROCEDURE GetClientesPrecoBilhete;

	-- ----------------------------------
	-- Deve ser possível enumerar todos os agentes de evento de um evento
	-- ----------------------------------
	DELIMITER $$
	CREATE PROCEDURE GetAgentes(IN nome_evento VARCHAR(50))
	BEGIN
		SELECT NOME,EMAIL FROM AGENTE AS A 
			INNER JOIN AGENTEEVENTO AS AE ON A.IDAGENTE = AE.IDAGENTE
				INNER JOIN EVENTO AS E ON AE.IDEVENTO = E.IDEVENTO
		WHERE E.DESIGNACAO = nome_evento
		ORDER BY NOME ASC;
	END
	$$

	CALL GetAgentes('Funk in Flores');

	-- -----------------------------------
	-- Caso seja preciso remover o procedimento
	-- -----------------------------------
	DROP PROCEDURE GetAgentes;

	-- -----------------------------------
	-- Deve ser possível listar a morada com o preço de aluguer mais alto para um evento
	-- -----------------------------------
	DELIMITER $$
	CREATE PROCEDURE GetMoradaMaisCara()
	BEGIN
		SELECT LOCALIDADE,RUA,PORTA,VALORALUGUER FROM MORADA AS M 
		INNER JOIN EVENTO AS E ON M.IDMORADA = E.IDMORADA
		ORDER BY E.VALORALUGUER DESC
		LIMIT 1;
	END
	$$

	CALL GetMoradaMaisCara();

	-- -----------------------------
	-- Caso seja preciso remover o procedimento
	-- -----------------------------
	DROP PROCEDURE GetMoradaMaisCara;

	-- -----------------------------
	-- Procedimento utilizado para saber qual o lucro total de um certo evento
	-- -----------------------------
	DELIMITER $$
	CREATE PROCEDURE GetRendaBilheteria(IN nome_evento VARCHAR(50))
	BEGIN
		SELECT SUM(CE.PRECOBILHETE) AS "Lucro Total" FROM CLIENTEEVENTO AS CE
		INNER JOIN EVENTO AS E ON CE.IDEVENTO = E.IDEVENTO
		WHERE E.DESIGNACAO = nome_evento AND CE.ESTADOPAGAMENTOBILHETE = '1';
	END
	$$
	CALL GetRendaBilheteria('Funk in Flores');

	-- -----------------------------
	-- Caso seja preciso remover o procedimento
	-- -----------------------------
	DROP PROCEDURE GetRendaBilheteria;

	-- -----------------------------
	-- Procedimento utilizado para saber qual a idade média dos clientes que vão a um certo evento
	-- -----------------------------
	DELIMITER $$
	CREATE PROCEDURE GetIdadeMedia(IN nome_evento VARCHAR(50))
	BEGIN
		SELECT ROUND(AVG(C.IDADE)) AS "Idade Média" FROM CLIENTE AS C
		INNER JOIN CLIENTEEVENTO AS CE ON CE.IDCLIENTE = C.IDCLIENTE
		INNER JOIN EVENTO AS E ON E.IDEVENTO = CE.IDEVENTO
		WHERE E.DESIGNACAO = nome_evento;
	END
	$$
	CALL GetIdadeMedia('Festival das Flores');

	-- ----------------------------
	-- Caso seja preciso remover o procedimento
	-- ----------------------------
	DROP PROCEDURE GetIdadeMedia;

	-- -------------------------------
	-- Indexes
	-- -------------------------------
	CREATE INDEX index_morada ON MORADA(IDMORADA);
	CREATE INDEX index_evento ON EVENTO(IDEVENTO);