-- Todos os clientes

SELECT IDCLIENTE,NOME,IDADE FROM CLIENTE;

-- Todos os eventos em uma determinada data

SELECT IDEVENTO,DESIGNACAO FROM EVENTO
	WHERE DATA = '2022-06-15 15:00';
    
-- Clientes que vão a um certo evento e tém um idade acima de 24 anos2

SELECT NOME,IDEVENTO,IDADE 
	FROM CLIENTE AS C 
		INNER JOIN CLIENTEEVENTO AS CE
			ON C.IDCLIENTE = CE.IDCLIENTE
	WHERE IDEVENTO = 2 AND IDADE > 24;

-- Agentes de um certo evento que foram pagos com um valor acima de 300 euros

SELECT NOME,DESIGNACAO,PRECOAGENTE 
	FROM AGENTE AS A 
		INNER JOIN AGENTEEVENTO AS AE 
			ON A.IDAGENTE = AE.IDAGENTE
	WHERE IDEVENTO = 1 AND PRECOAGENTE > 300 AND ESTADOPAGAMENTOAGENTE = 1