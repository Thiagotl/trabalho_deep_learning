
# VARIAVEIS 
# "CLIENTE"| Código identificador do cliente na base de dados. Não deve ser usado como variável explicativa, pois funciona apenas como identificação.
# "STATUSx"| Variável com a classificação do cliente como "BOM" ou "MAU". Como contém a mesma informação da variável resposta, deve ser removida da modelagem para evitar vazamento de informação.
# "STATUS"| Variável resposta do problema. Indica se o cliente é classificado como "BOM" ou "MAU".
# "IDADE"| Idade do cliente. É uma variável numérica utilizada como característica explicativa do perfil do cliente.
# "BIDADE_1"| Variável binária derivada da idade, indicando pertencimento a uma determinada faixa etária.
# "BIDADE_2"| Variável binária derivada da idade, indicando outra faixa etária.
# "BIDADE_3"| Variável binária derivada da idade, indicando outra faixa etária.
# "BIDADE_4"| Variável binária derivada da idade, indicando outra faixa etária.
# "UNIFED"| Unidade federativa do cliente, como "SP", "RJ" ou "OUTROS".
# "BSTATE_SP"| Variável binária que indica se o cliente pertence ao estado de São Paulo. Assume valor 1 para "SP" e 0 caso contrário.
# "BSTATE_RJ"| Variável binária que indica se o cliente pertence ao estado do Rio de Janeiro. Assume valor 1 para "RJ" e 0 caso contrário.
# "FONE"| Indica se o cliente possui telefone cadastrado, assumindo valores como "SIM" ou "NAO".
# "BFONE"| Variável binária derivada de "FONE", indicando presença ou ausência de telefone cadastrado.
# "INSTRU"| Grau de instrução do cliente. Na base aparecem categorias como "PRIM & SEC", "SUP" e "MV".
# "BINST_PR_SEC"| Variável binária que indica se o cliente possui instrução primária ou secundária.
# "BINST_SUP"| Variável binária que indica se o cliente possui ensino superior.
# "CARTAO"| Indica se o cliente possui cartão, assumindo valores como "SIM" ou "NAO".
# "BCARD"| Variável binária derivada de "CARTAO", indicando presença ou ausência de cartão.
# "RESTR"| Indica se o cliente possui algum tipo de restrição cadastral ou financeira, assumindo valores como "SIM" ou "NAO".
# "BRESTR"| Variável binária derivada de "RESTR", indicando a presença ou ausência de restrição.
# "RESID"| Tipo de residência do cliente. Na base aparecem categorias como "PROP", para residência própria, e "ALUG", para residência alugada.
# "BRESID"| Variável binária derivada de "RESID", indicando o tipo de residência do cliente, especialmente se possui residência própria.
# "ATRASO"| Quantidade de atraso observada para o cliente. Essa variável deve ser tratada com cuidado, pois pode representar informação posterior à concessão de crédito e gerar vazamento de informação. Por isso, recomenda-se removê-la da modelagem preditiva.
# "FICÇÃO"| Indica se o cliente comprou ou possui interesse em livros de ficção, assumindo valores como "SIM" ou "NAO".
# "BFIX"| Variável binária derivada de "FICÇÃO", indicando presença ou ausência de compra/interesse em ficção.
# "NÃOFICÇAO"| Indica se o cliente comprou ou possui interesse em livros de não ficção, assumindo valores como "SIM" ou "NAO".
# "BNOFIX"| Variável binária derivada de "NÃOFICÇAO", indicando presença ou ausência de compra/interesse em não ficção.
# "AUTOAJUDA"| Indica se o cliente comprou ou possui interesse em livros de autoajuda, assumindo valores como "SIM" ou "NAO".
# "BAUTAJ"| Variável binária derivada de "AUTOAJUDA", indicando presença ou ausência de compra/interesse em autoajuda.
# "CATEG"| Variável categórica/binária associada ao perfil ou categoria do cliente na base.

descritiva <- summarise(dados)

table(dados$status)
table(dados$unifed)
table(dados$cartao)
table(dados$restr)
table(dados$resid)
table(dados$instru)
table(dados$atraso)
