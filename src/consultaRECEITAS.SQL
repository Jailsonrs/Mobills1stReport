BEGIN TRANSACTION
    SELECT 
        Sexo, 
        Cidade, 
        Estado,
        Profissao,
        DataNascimento,
        UsuarioId
    FROM dbo.PerfilClientes  
    WHERE  dataNascimento >= '19950101' AND  dataNascimento <= '20031231' 
COMMIT

BEGIN TRANSACTION
    SELECT * FROM dbo.Receitas 
    WHERE DataReceitaIndice >= '20190711' AND DataReceitaIndice <= '20191011'
    AND Ativo = 1
    AND UsuarioId IN ( SELECT UsuarioId FROM dbo.PerfilClientes  
    WHERE  dataNascimento >= '19950101' AND  dataNascimento <= '20031231' )
COMMIT

BEGIN TRANSACTION
    SELECT * FROM dbo.Despesas 
    WHERE DataDespesaIndice >= '20190711' AND DataDespesaIndice <= '20191011'
    AND Ativo = 1
    AND UsuarioId IN ( SELECT UsuarioId FROM dbo.PerfilClientes  
    WHERE  dataNascimento >= '19950101' AND  dataNascimento <= '20031231' )
COMMIT
