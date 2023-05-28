#include <oxstd.oxh>
#import <packages/PcGive/pcgive_ects>
#include <Classes/Cointegration.ox>

//	mPhi: matrix dos coeficientes. Inicialmente uma matriz zerada [iQtdVarDependente x iQtdVarDependente*iQtdLags];
//	iValue: vParamValues - Valor efetivo dos parametros
//	sName: asParamNames - Nome dos parametros da regressao do Ox
//	iQtdLags: Quantidade de lags
//	iRegDependente: Regiao dependente na regressao
//	sVarSufix: Sufixo do nome da variavael "D_R"
//	asTipo: variaveis dependentes ex: asTipo = {"Admitidos", "Desligados"};
ProcessoPhi(mPhi, const iValue, const sName, const iQtdLags, const iRegDependente, const sVarSufix, const asTipo) {
    println("Processo Phi iniciado");
    decl iContRows, iContCols, iContVar, sParamName, index;


    for (iContRows = 0; iContRows < sizeof(asTipo); ++iContRows) {
        // Preenchimento da matrix Lambda
        for (iContCols = 0; iContCols < iQtdLags; ++iContCols) {
            for (iContVar = 0; iContVar < sizeof(asTipo); ++iContVar) {

				// O Ox tem uma nomenclatura diferente quando temos regressao univariada
				// por isso o tratamento abaixo
				if(sizeof(asTipo) == 1)
				{
				sParamName = sprint(sVarSufix, iRegDependente, "_", asTipo[iContVar], "_", iContCols + 1);
				}
				else
				{
				// determina o nome das variaveis
                sParamName = sprint(sVarSufix, iRegDependente, "_", asTipo[iContVar], "_", iContCols + 1, "@", sVarSufix, iRegDependente, "_", asTipo[iContRows]);
				}

				//println("Parametro procurado: ", sParamName);
                index = strfind(sName, sParamName);
				//println("index: ", index);

                if (index > -1) {
                    mPhi[iContRows][(sizeof(asTipo)*iContCols) + iContVar] = iValue[index];
                }
            } // Fim: iContVar (looping nos tipos para as colunas (colunas pares e impares))
        } // Fim: iContCols (looping nos lags)
    } // Fim: iContRows (looping nos tipos para as linhas)
    println("Processo Phi finalizado");
	//println(mPhi);
    return mPhi;
}


//	mLambda: matrix dos coeficientes. Inicialmente uma matriz zerada [iQtdVarDependente x iQtdVarDependente*(iQtdLags+1)]
//	iValue: vParamValues - Valor efetivo dos parametros
//	sName: asParamNames - Nome dos parametros da regressao do Ox
//	iQtdLags: Quantidade de lags
//	iRegDependente: Regiao dependente na regressao
//	sVarSufix: Sufixo do nome da variavael star "D_R"
//	sVarSufix2: Sufixo do nome da variavael dependente "D_R"
//	asTipo: variaveis dependentes ex: asTipo = {"Admitidos", "Desligados"};
ProcessoLambda(mLambda, const iValue, const sName, const iQtdLags, const iRegDependente, const sVarSufix, const sVarSufix2, const asTipo) {
    println("Processo Lambda iniciado");
    decl iContRows, iContCols, iContVar, sParamName, index;

    for (iContRows = 0; iContRows < sizeof(asTipo); ++iContRows) {
        // Preenchimento da matrix Lambda
        for (iContCols = 0; iContCols <= iQtdLags; ++iContCols) {
            for (iContVar = 0; iContVar < sizeof(asTipo); ++iContVar) {
                // determina o nome das variaveis
                if (iContCols == 0) {
                    // Determina o nome do parametro sem lag

					// O Ox tem uma nomenclatura diferente quando temos regressao univariada
					// por isso o tratamento abaixo
					if(sizeof(asTipo) == 1)
					{
	                    sParamName = sprint(sVarSufix, "star_", asTipo[iContVar]);
					}
					else
					{
	                    sParamName = sprint(sVarSufix, "star_", asTipo[iContVar], "@", sVarSufix2, iRegDependente, "_", asTipo[iContRows]);
					}
                } else {
                    // Determina o nome do parametro COM lag

					// O Ox tem uma nomenclatura diferente quando temos regressao univariada
					// por isso o tratamento abaixo
					if(sizeof(asTipo) == 1)
					{
						sParamName = sprint(sVarSufix, "star_", asTipo[iContVar], "_", iContCols);
					}
					else
					{
	                    sParamName = sprint(sVarSufix, "star_", asTipo[iContVar], "_", iContCols, "@", sVarSufix2, iRegDependente, "_", asTipo[iContRows]);
					}

				}

				//println("Parametro procurado: ", sParamName);
                index = strfind(sName, sParamName);
				//println("index: ", index);

                if (index > -1) {
                    mLambda[iContRows][(sizeof(asTipo)*iContCols) + iContVar] = iValue[index];
                }
            } // Fim: iContVar (looping nos tipos para as colunas (colunas pares e impares))
        } // Fim: iContCols (looping nos lags)
    } // Fim: iContRows (looping nos tipos para as linhas)
    println("Processo Lambda finalizado");
    //println(mLambda);
    return mLambda;
}


//	mU: matriz de coeficientes para ser preenchida (inicialmente deve ser uma matriz de zeros)
//	iValue: vParamValues - Valor efetivo dos parametros
//	sName: asParamNames - Nome dos parametros
//	iQtdLags: Quantidade de lags
//	iRegDependente: Regiao dependente na regressao
//	sVarSufix: Sufixo do nome da variavael "D_R"
//	asTipo: variaveis dependentes ex: asTipo = {"Admitidos", "Desligados"};
//	iQtdVarCSeasonal: quantidade de variaveis sazonais
ProcessoU(mU, const iValue, const sName, const iQtdLags, const iRegDependente, const sVarSufix, const asTipo, const iQtdVarCSeasonal) {
    println("Processo U iniciado");
    decl iContRows, iContCols, iContVar, sParamName, index, asConstants;

	asConstants = {"Constant"};

	// Constroi o vetor de dummies sazonais ex: asConstants = {"Constant", "CSeasonal", "CSeasonal_1", "CSeasonal_2", "CSeasonal_3"};
	for (iContRows = 0; iContRows < iQtdVarCSeasonal; ++iContRows) {
		if(iContRows == 0){
			asConstants = asConstants | {"CSeasonal"};
		} else {
			asConstants = asConstants | {sprint("CSeasonal_", iContRows)};
		}
	}
	//println("asConstants:", asConstants);
	
    for (iContRows = 0; iContRows < sizeof(asTipo); ++iContRows) {
        // Preenchimento da matrix Lambda
        for (iContCols = 0; iContCols < sizeof(asConstants); ++iContCols) {

			// determina o nome das variaveis
			// O Ox tem uma nomenclatura diferente quando temos regressao univariada
			// por isso o tratamento abaixo
			if(sizeof(asTipo) == 1)
			{
            	sParamName = sprint(asConstants[iContCols]);
			}
			else
			{
        	    sParamName = sprint(asConstants[iContCols], "@", sVarSufix, iRegDependente, "_", asTipo[iContRows]);
			}
			
            index = strfind(sName, sParamName);

            if (index > -1) {
                mU[iContRows][iContCols] = iValue[index];
            }
        } // Fim: iContCols (looping nos lags)
    } // Fim: iContRows (looping nos tipos para as linhas)
    println("Processo U finalizado");
    return mU;
}

//	iValue: vParamValues - Valor efetivo dos parametros
//	sName: asParamNames - Nome dos parametros
//	iRegDependente: Regiao dependente na regressao
//	sVarSufix: Sufixo do nome da variavael "D_R"
// 	anoIni: (nao utilizado mais) era utilizado para construcao do vetor de dummies.
// 	anoFim: (nao utilizado mais) era utilizado para construcao do vetor de dummies.
//	asTipo: variaveis dependentes ex: asTipo = {"Admitidos", "Desligados"};
ProcessoIIS(const iValue, const sName, const iRegDependente, const sVarSufix, const anoIni, const anoFim, const asTipo) {
    //(const iValue, const sName, const iQtdLags, const iRegDependente)
    println("Processo Extracao da matriz de saturacao (IIS)");
    decl nContTipo, nContTotal, whatToLookFor, mReturn, index;

	#include "./IIS_NAMES_Config.ox"

	// Matriz IIS
	// Inicializa uma matriz Zerada Zeros(linhas, colunas)
	// mReturn = zeros(columns(asTipo), ((anoFim - anoIni) + 1) * 12);
	mReturn = zeros(columns(asTipo), sizeof(asListaDummies));

    //Faz um looping por todas as datas, procura a respectiva dummy e se achar adiciona o valor a tabela.
    for (nContTotal = 0; nContTotal < sizeof(asListaDummies); ++nContTotal) {
            //println(sprint("I:",nCountAno,"(",nCountMes,")"));
            for (nContTipo = 0; nContTipo < sizeof(asTipo); ++nContTipo) {


				// O Ox tem uma nomenclatura diferente quando temos regressao univariada
				// por isso o tratamento abaixo
				if(sizeof(asTipo) == 1)
				{
	            	whatToLookFor = sprint(asListaDummies[nContTotal]);
				}
				else
				{
	        	    whatToLookFor = sprint(asListaDummies[nContTotal], "@", sVarSufix, iRegDependente, "_" , asTipo[nContTipo]);
				}
			
				//println("whatToLookFor: ", whatToLookFor);
				index = find(sName, whatToLookFor);

                // caso tenha achado o indice atualiza a tabela
                if (index >= 0) {
                    mReturn[nContTipo][nContTotal] = iValue[index];
                }
            }
    }
	//println(mReturn);
    return mReturn;
}

//	iValue: vParamValues - Valor efetivo dos parametros
//	sName: asParamNames - Nome dos parametros
//	iRegDependente: Regiao dependente na regressao
//	sVarSufix: Sufixo do nome da variavael "D_R"
//	asTipo: variaveis dependentes ex: asTipo = {"Admitidos", "Desligados"};
//	iQtdVarDependente: quantidade de variaveis dependentes (geralmente determinado programaticamente no config.ini);
ProcessoPILongRun(const iValue, const sName, const iRegDependente, const sVarSufix, const asTipo, const iQtdVarDependente) {
    //(const iValue, const sName, const iQtdLags, const iRegDependente)
    println("Processo Extracao da matriz de longo prazo");
    decl nContTipo, nContTotal, iContVar, sParamName, nCountAno, nCountMes, mReturn, index;
    mReturn = zeros(iQtdVarDependente, iQtdVarDependente);

	//println("mReturn ProcessoPILongRun", mReturn);

    for (nContTipo = 0; nContTipo < sizeof(asTipo); ++nContTipo) {

		// println(sprint("betaZ_", "1", "@", sVarSufix, iRegDependente, "_" , asTipo[nContTipo]));

		// O Ox tem uma nomenclatura diferente quando temos regressao univariada
		// por isso o tratamento abaixo
		if(sizeof(asTipo) == 1)
		{
			index = find(sName, sprint("betaZ_", "1"));
		} 

		/****************
		// ToDo: ESSE PROCESSO ACREDITO QUE PODE SER AUTOMATIZADO.
		// CONTUDO POR PRATICIDADE ISSO FICA PARA UMA VERSAO POSTERIOR.
		// POR ENQUANTO VAI SER MANUAL NA QUANTIDADE DE VETORES DE COINTEGRACAO POSSIVEIS.
		****************/

		index = find(sName, sprint("betaZ_", "1", "@", sVarSufix, iRegDependente, "_" , asTipo[nContTipo]));
		
        // caso tenha achado o indice atualiza a tabela
        if (index >= 0) {
            mReturn[nContTipo][0] = iValue[index];
        }

        index = find(sName, sprint("betaZ_", "2", "@", sVarSufix, iRegDependente, "_" , asTipo[nContTipo]));
        // caso tenha achado o indice atualiza a tabela
        if (index >= 0) {
            mReturn[nContTipo][1] = iValue[index];
        }

		index = find(sName, sprint("betaZ_", "3", "@", sVarSufix, iRegDependente, "_" , asTipo[nContTipo]));
        // caso tenha achado o indice atualiza a tabela
        if (index >= 0) {
            mReturn[nContTipo][1] = iValue[index];
        }
    }
	// println(mReturn);
    return mReturn;
}

//	iValue: vParamValues - Valor efetivo dos parametros
//	sName: asParamNames - Nome dos parametros
//	iQtdLags: Quantidade de lags
//	iRegDependente: iCont - identificador da variavel dependente
//	sVarSufix: Sufixo do nome da variavael "D_R"
//	sMacroSufix: Sufixo do nome da variavael macro {"D"}
//	aMacoVarNames: Nome d variavel macro
//	asTipo: variaveis dependentes ex: asTipo = {"Admitidos", "Desligados"};
//	iQtdVarDependente: quantidade de variaveis dependentes (geralmente determinado programaticamente no config.ini);
ProcessoMacroVariables(const iValue, const sName, const iQtdLags, const iRegDependente, const sVarSufix, const sMacroSufix, const aMacoVarNames, const asTipo, const iQtdVarDependente) {
    //vParamValues, asParamNames, iQtdLags, iCont, sVarSufix, aMacoVarNames
    println("Processo Extracao das variaveis macroeconomicas (matrix de longo Prazo inclusive)");
    decl nContTipo, nContLag, nContVar, nContTotal, mReturn, sParamName, index;

    mReturn = zeros(iQtdVarDependente, (rows(aMacoVarNames) * (iQtdLags + 1)));

    for (nContTipo = 0; nContTipo < sizeof(asTipo); ++nContTipo) {
        nContTotal = 0;

        for (nContLag = 0; nContLag <= iQtdLags; ++nContLag) {
            for (nContVar = 0; nContVar < rows(aMacoVarNames); ++nContVar) {
                if (nContLag == 0) {
					// Determina o nome do parametro sem lag
	 				// O Ox tem uma nomenclatura diferente quando temos regressao univariada
	 				// por isso o tratamento abaixo
					if(sizeof(asTipo) == 1)
					{
	                    sParamName = sprint(sMacroSufix, aMacoVarNames[nContVar]);
					}
					else
					{
	                    sParamName = sprint(sMacroSufix, aMacoVarNames[nContVar], "@", sVarSufix, iRegDependente, "_", asTipo[nContTipo]);
					}				 
				} else {
					// Determina o nome do parametro COM lag
					// O Ox tem uma nomenclatura diferente quando temos regressao univariada
	 				// por isso o tratamento abaixo
					if(sizeof(asTipo) == 1)
					{
	                    sParamName = sprint(sMacroSufix, aMacoVarNames[nContVar], "_", nContLag);
					}
					else
					{
	                    sParamName = sprint(sMacroSufix, aMacoVarNames[nContVar], "_", nContLag, "@", sVarSufix, iRegDependente, "_", asTipo[nContTipo]);
					}
				}

                //println(sParamName);
                index = find(sName, sParamName);

                // caso tenha achado o indice atualiza a tabela
                if (index >= 0) {
                    mReturn[nContTipo][nContTotal] = iValue[index];
                }

                ++nContTotal;
            } // fim nContVar
        } // fim nContLag
    } // fim nContTipo

	// println("%r", asTipo, "%c", {"D_Selic", "D_IPCA", "D_PIM", "D_Selic_1", "D_IPCA_1", "D_PIM_1", "D_Selic_2", "D_IPCA_2", "D_PIM_2", "betaMacro"},  mReturn);
    //println(mReturn);
	//println("mReturn ProcessoMacroVariables", mReturn);
    return mReturn;
}

GetRegionNames(const iQtdRegioes, const sVarPrefix, const sVarPosfix) {
    // println("iQtdRegioes: ", iQtdRegioes);
    // println("sVarPrefix: ", sVarPrefix);
    // println("sVarPosfix: ", sVarPosfix);
    decl nCont, aNames;

    for (nCont = 1; nCont <= iQtdRegioes; ++nCont) {
        if (nCont == 1) {
            aNames = {sprint(sVarPrefix, nCont, sVarPosfix)};
        } else {
            aNames = aNames ~ {sprint(sVarPrefix, nCont, sVarPosfix)};
        }
    }
    return aNames;
}


main() {
    // Arquivo de configuracao
#include "./Config.ini"

    /***************************************************
     *
     * Declaração de variaveis de configuracao do script
     *
     *************************************************** */
	 
    // Variáveis do programa
    decl i, sVarSufix;
	
    //println("Carregando dados de macrovariaveis");
    //decl mMacroData;
    //decl daBaseMacro = new Database();
    //daBaseMacro.Load(txDbaseMacroVariables);
    //print( "%c", daBaseMacro.GetAllNames(), "%cf", daBaseMacro.GetAll());
    //println(" Carregando dados das colunas: ", aMacroVarNames);
    //mMacroData = daBaseMacro.GetVar(aMacroVarNames);
    //print( "%c", aMacroVarNames, "%cf", mMacroData[0:9][]);
    //delete daBaseMacro;
    //println("Macrovariaveis carregadas");
    //println("Carregando matrix de pessos W");

	/* ********* CARREGANDO MATRIX DE PESOS ********* */
    println("Carregando matrix de pessos W");
	decl mW;
	println("Matriz de pesos utilizada: ", sprint(txMatPathW_Matrix, strMatrixDePesos));
    mW = loadmat(sprint(txMatPathW_Matrix, strMatrixDePesos));
    //println("mW", mW);
	
	println("*** Iniciando estimacao dos modelos *** \n");
    // iCont : Contador da regiao atual
    // iCont2: Contador
    decl iCont;

	// Carrega a informação de qual o rank de cada região 
	decl mRankRegions;
	println("Lista de ranks das regioes: ", sprint("./mat_files/", "rankOfRegions.mat"));
    mRankRegions = loadmat(sprint("./mat_files/", "rankOfRegions.mat"));
    //println(mRankRegions);
		
    for (iCont = 1; iCont <= iQtdRegioes; ++iCont) {

		//println("mRankRegions has ", rows(mRankRegions), " rows and ", columns(mRankRegions), " cols");
		// continue;
        // FOR DEBUG ONLY
        // if(iCont >2){
        //     exit(0);
        // }


        // print Headder
        println("\n\n*****************************************");
        println("             Regiao ", iCont);
        println("*****************************************\n\n");

		println("Rank: ", mRankRegions[iCont-1][0]);
		
        // Inicio um nomo objeto do tipo PcGive
        println("Iniciando um nomo objeto do tipo PcGive referente a regiao ", iCont);
        decl model = new PcGive();

		println("\nCarregando base de dados para regiao ", iCont);
        model.Load(txDbase);

		println("\tPeriodo da base de dados");
        println("\tData inicial: ", model.GetYear1(), "-", model.GetPeriod1());
        println("\tData final: ", model.GetYear2(), "-", model.GetPeriod2());

		/* ********* CONTRUCAO DAS VARIAVEIS ESTRELA (EXTERNAS) ********* */
		// As Variaveis Star sao uma combinacao linear das variaveis esternas.
        println("(1) Iniciando construcao das variaveis star para a regiao ", iCont);

		decl mData, beta;

        //mData = model.GetAll();

		println("\tAdicionando variavel star da regiao ", iCont);
        // println(GetRegionNames(iQtdRegioes, "R", "_Desligados"));
        decl iContador;
        for (iContador = 0; iContador < columns(aVarDependenteNames); ++iContador) {
			println("\tAdicionando variavel star da regiao ", iCont, " (", aVarDependenteNames[iContador],")");
			//println(GetRegionNames(iQtdRegioes, "R_", sprint("_", aVarDependenteNames[iContador])));
            mData = model.GetVar(GetRegionNames(iQtdRegioes, "R_", sprint("_", aVarDependenteNames[iContador])));
           	model.Append(mData * mW[][iCont - 1], sprint("star_", aVarDependenteNames[iContador]));
			//println("%c", GetRegionNames(iQtdRegioes, "R_", aVarDependenteNames[iContador]), mData[0:6][]);
        }
        println("\tConcluido construcao das variaveis star para a regiao ", iCont);

		/* ********* CONTRUCAO DAS VARIAVEIS EM PRIMEIRA DIFERENCA ********* */
        println("(2) Iniciando construcao da variavel Delta para a regiao ", iCont);

		for(iContador = 0; iContador < columns(aVarDependenteNames); ++iContador) {
            // Adiciona a variavel em primeira Diferenca
			println("\tAdicionando variavel Delta da regiao ", iCont, " (", aVarDependenteNames[iContador],")");
            mData =	model.GetVar(sprint("R_", iCont, "_", aVarDependenteNames[iContador]));
            model.Append(diff(mData), sprint("D_R_", iCont, "_", aVarDependenteNames[iContador]));

            // Adiciona a variavel Star em primeira diferenca
  			println("\tAdicionando variavel Delta* da regiao ", iCont, " (", sprint("star_", aVarDependenteNames[iContador]),")");
            mData =	model.GetVar(sprint("star_", aVarDependenteNames[iContador]));
            model.Append(diff(mData), sprint("D_star", "_", aVarDependenteNames[iContador]));
        }
		
        // CONTRUCAO DAS VARIAVEIS DELTA
        println("\tConcluido construcao da variavel Delta para a regiao ", iCont);

		
		/* ********* CONTRUCAO DA MATRIZ DE LONGO PRAZO ********* */
        println("(3) Iniciando construcao da variavel beta*Z (Cointegracao) para a regiao ", iCont);

		// Leitura do vetor de cointegracao
        beta = loadmat(sprint(txCoIntMatPath, sprint("Weak2_CoInt_R", iCont, ".mat")));
		//println("beta: ", beta);

		for (iContador = 0; iContador < columns(aVarDependenteNames); ++iContador) {
			if(iContador ==0){
				mData =	model.GetVar(sprint("R_", iCont, "_", aVarDependenteNames[iContador]));
            } else {
				mData = mData ~ model.GetVar(sprint("R_", iCont, "_", aVarDependenteNames[iContador]));
			}
        }

		for (iContador = 0; iContador < columns(aVarDependenteNames); ++iContador) {
			mData = mData ~ model.GetVar(sprint("star_", aVarDependenteNames[iContador]));
		}
		//println("%c", GetRegionNames(iQtdRegioes, "R_", aVarDependenteNames[iContador]), mData[0:6][]);

        decl asBetaZ;
        for(decl i=1; i<=rows(beta); i++){
            if(i==1){
                asBetaZ =  {sprint("betaZ_", i)};    
            } else {
                asBetaZ =  asBetaZ ~ {sprint("betaZ_", i)};    
            }
            // println("rows: ", asBetaZ);
        }

        model.Append(mData * beta', asBetaZ);
		
	
//		model.SaveIn7(sprint("BASEDEBUG_", iCont, "_Fulldatabase"));
	    println("\tConcluido construcao da variavel beta*Z (Cointegracao) para a regiao ", iCont);

		/* ********* ADICAO DAS VARIAVEIS MACROECONOMICAS ********* */
        println("(4) Iniciando adicao de macrovariaveis para a regiao ", iCont);
        // Leitura do vetor de cointegracao
        // println("\tAdicao de macrovariaveis em nivel para a regiao ", iCont);
        // println(mMacroData[1:10][]);
		
        //model.Append(mMacroData, aMacroVarNames);

		println("\tAdicao de macrovariaveis em primeira differenca para a regiao ", iCont);
        for (i = 0; i < rows(aMacroVarNames); ++i) {
			print("\tAdicionando variavel");
		    mData =	model.GetVar(sprint(aMacroVarNames[i]));
            model.Append(diff(mData), sprint("D_", aMacroVarNames[i]));
        }
		
        //if (columns(mMacroData) > 1) {
			// ****************
			// Processo comentado pois foi verificado que nao existe cointegracao entre as variaveis externas e as macrovariaveis.

			// println("(5) Adicionando matriz de longo prazo das Macrovariaveis para a regiao ", iCont);
            // beta = loadmat(sprint(txCoIntMatPath, sprint("CoInt_MacroVar.mat")));
            // model.Append(mMacroData * beta', {"betaMacro"});
            // println("\tConcluido adicao de macrovariaveis para a regiao ", iCont);
        //}

        // Apago variaveis que nao serao mais utilizadas
        delete mData, beta;

		/*
		Neste ponto o banco de dados ja contem todas as variaveis para serem estimadas.
        Tanto variaveis em niveis quanto em primeira diferenca
		*/
        //model.SaveIn7(sprint("R_", iCont, "_database(DEBUG)"));

		/* ********* INICIAMOS A MODELAGEM DO VAR PARA ESTIMACAO ********* */
        println("(6) Construindo modelo de estimacao da regiao ", iCont);

		//model.Info();
        model.SetModelClass("SYSTEM");

		// Deseleciona as variaveis
        model.DeSelect();

		model.Deterministic(3);
        /* ********* 
		iCseason
        in: int:
        	-1: no seasonals;
        	 0: n Seasonals Season, Season_1, ...;
        	 1: n centred seasonals CSeason, CSeason_1;
        	 2: 1 seasonal called Seasonal;
        	 3: 1 centred seasonal called CSeasonal.
        Appends constant, trend and seasonals to the database. These variables are named
		Constant, Trend and Season. Season_1, ..., Season_x, where x is the frequency.
        
        Season has a 1 in quarter 1 (for quarterly data), and zeros elsewhere, Season_1 has a 1 in quarter 2, etc.
        If iCseason is 0, normal seasonals are created. If iCseason is 1, the seasonals are centred
		(with quarterly observations, for quarter 1: 0.75, -0.25, -0.25, -0.25, ...),
		in which case the names are CSeason, CSeason_1, ..., CSeason_x. No seasonals are created if iCseason is < 0.
		********* */
		
        // Vamos utilizar o sufixo "D_" para o modelo em primeira diferenca
        //sVarSufix = "D_";
		sVarSufix = "";
		
        //println(model.GetAllNames());

		// adiciona variavel dependente (Adminitidos e Desligados)
		for(iContador = 0; iContador < columns(aVarDependenteNames); ++iContador) {
            // Adiciona a variavel em primeira Diferenca
			println("\tAdicionando variavel dependente da regiao ", iCont, " (", aVarDependenteNames[iContador],")");
			model.Select("Y", {sprint(sVarSufix, "R_", iCont, "_", aVarDependenteNames[iContador]), 0, 0});
        }

		if (iQtdLags > 0) {
            // adiciona a variavel independente (lag da independente)
			for(iContador = 0; iContador < columns(aVarDependenteNames); ++iContador) {
            // Adiciona a variavel em primeira Diferenca
			println("\tAdicionando variavel independente (lag da independente) da regiao ", iCont, " (", aVarDependenteNames[iContador],")");
			model.Select("X", {sprint(sVarSufix, "R_", iCont, "_", aVarDependenteNames[iContador]), 1, iQtdLags});
	        }
        }

        // Adiciona a variavel "star"
		for(iContador = 0; iContador < columns(aVarDependenteNames); ++iContador) {
            // Adiciona a variavel em primeira Diferenca
			println("\tAdicionando variavel star da regiao ", iCont, " (", aVarDependenteNames[iContador],")");
			model.Select("X", {sprint(sVarSufix, sprint("star_", aVarDependenteNames[iContador])), 0, iQtdLags});
	    }
	
		// Adiciona a variavel "betaZ" com um lag apenas (representação da matriz de longo prazo)
		if(mRankRegions[iCont-1][0] > 0){
			model.Select("X", asBetaZ);
		}

			//println("mRankRegions has ", rows(mRankRegions), " rows and ", columns(mRankRegions), " cols");
		// continue;
        // FOR DEBUG ONLY
        // if(iCont >2){
        //     exit(0);
        // }


        // Adiciona a variaveis macroeconomicas com um lag apenas (representação da matriz de longo prazo)
        //decl nContVarMacro;
        for (i = 0; i < rows(aMacroVarNames); ++i) {
            model.Select("X", {sprint(sVarSufix, aMacroVarNames[i]), 0, iQtdLags});
            println("\tAdicionado: ", sprint(sVarSufix, aMacroVarNames[i]));
        }
	
        // Adiciona variaveis constante e sesonals
		println("\tAdicionado: Constant");
        model.Select("U", {"Constant", 0, 0});

		if (is_DUMMY_ON) {
			if(type_DUMMY == "U"){
				println("\tAdicionado: CSeasonal U");
		        model.Select("U", {"CSeasonal", 0, iQtdVarCSeasonal-1});
			}else if(type_DUMMY == "X"){
				println("\tAdicionado: CSeasonal X");
				model.Select("X", {"CSeasonal", 0, iQtdVarCSeasonal-1});
			} else {
				println("\n\t >>>> NAO TEM ESSA CONFIGURACAO DE DUMMY <<<");
				exit(0);
			}
        }

	
        // determina a janela de tempo do modelo
        //model.SetSelSampleByIndex(1, 227);
		//model.SetSelSampleByDates(200201, 202208);
		model.SetSample(2006, 7, 2023, 04);


        // Liga o autometrics
		// (Mudar flag para TRUE, para estimar todos modelos com IIS)
		//	ANTIGA CONDICAO:	(iCont == 69) || (iCont == 84) || (iCont == 99) || TRUE
        if (is_IIS_ON) {
			println("Processo IIS LIGADO para esta regiao");
            model.Autometrics(IIS_pvalue, IIS_type, 1);
        } else {
            //model.Autometrics(0.0001, "IIS", 1);
			println("Processo IIS desligado para esta regiao");
        }

		// Desliga a impressao em tela do Autometrics
        model.AutometricsSet("print", 1);
        // determina o metodo de estimacao.
        model.SetMethod(M_OLS);
        // Realiza a estimacao do modelo
		println("INICIANDO ESTIMACAO");
		model.SetPrint(TRUE);
        model.Estimate();

		
		// mostra os criterios de informacao dos modelos
		decl mInfoCrit;
		mInfoCrit = model.InformationCriteria();
		print("%c", {"AIC", "SC", "HQ", "FPE"}, "%cf", mInfoCrit);
		println(">>> AIC: ", mInfoCrit[0]);
		println(">>> SC: ", mInfoCrit[1]);
		println(">>> HQ: ", mInfoCrit[2]);

        println("Fazendo aquisicao de parametros");

		// Declaro as matrizes Phi e Lambda
        // Phi: Matrix de coeficientes do log da dependente
        // Lambda: Matrix de coeficientes das star
        // U: Matrix de coeficientes da constante e das dummies sazonais
        decl mPhi, mLambda, mLambda_0, mU, iContParam, iTotalParam, asParamNames, vParamValues, nContLags, A_0, A_L, mIIS, mAlpha, mD_macro;

		// inicia as matizes
        mPhi = zeros(iQtdVarDependente, iQtdVarDependente * iQtdLags);
        mLambda = zeros(iQtdVarDependente, iQtdVarDependente * (iQtdLags + 1));
        mU = zeros(iQtdVarDependente, (1 + iQtdVarCSeasonal));	//1: constante - 11:Seasonal

		// inicializa o total de parametros
        iTotalParam = model.GetParCount();
        // inicializa um vetor com o nome dos parametros
        asParamNames = model.GetParNames();
        // inicializa um vetor com o valor dos parametros
        vParamValues = model.GetPar();

        //Impressao dos parametros em Tela (Mudar flag para TRUE, para imprimir todos os modelos)
        if (FALSE) {
            print("%r", asParamNames,
                  "%c", {"Coef", "Std.Err"}, "%14.4f", vParamValues ~ model.GetStdErr());
        }
		
        // Salva a matriz de IIS Para o modelo
        mIIS = ProcessoIIS(vParamValues, asParamNames, iCont, sprint(sVarSufix, "R_"), model.GetYear1(), model.GetYear2(), aVarDependenteNames);
		//println("mIIS", mIIS);
		//println("mIIS_", rows(mIIS), "x", columns(mIIS));
		savemat(sprint(txMatPathRawMatrix, sVarSufix, "R", iCont, "_IIS.mat"), mIIS);

		mAlpha = ProcessoPILongRun(vParamValues, asParamNames, iCont, sprint(sVarSufix, "R_"), aVarDependenteNames, iQtdVarDependente);
		//println("mAlpha", mAlpha);
		savemat(sprint(txMatPathRawMatrix, sVarSufix, "R", iCont, "_Alpha.mat"), mAlpha);

		// Completa os valores da matrix Lambda
        mLambda = ProcessoLambda(mLambda, vParamValues, asParamNames, iQtdLags, iCont, sVarSufix, sprint(sVarSufix, "R_"), aVarDependenteNames);
		//println("mLambda", mLambda);
		savemat(sprint(txMatPathRawMatrix, sVarSufix, "R", iCont, "_Lambda.mat"), mLambda);
		
		mPhi = ProcessoPhi(mPhi, vParamValues, asParamNames, iQtdLags, iCont, sprint(sVarSufix, "R_"), aVarDependenteNames);
		//println("mPhi", mPhi);
		savemat(sprint(txMatPathRawMatrix, sVarSufix, "R", iCont, "_Phi.mat"), mPhi);

		mU = ProcessoU(mU, vParamValues, asParamNames, iQtdLags, iCont, sprint(sVarSufix, "R_"), aVarDependenteNames, iQtdVarCSeasonal);
		//print("mU", mU);
        savemat(sprint(txMatPathRawMatrix, sVarSufix, "R", iCont, "_U.mat"), mU);

		// const iValue, const sName, const iRegDependente, const sVarSufix, const aMacroVarNames
        mD_macro = ProcessoMacroVariables(vParamValues, asParamNames, iQtdLags, iCont, sprint(sVarSufix, "R_"), sVarSufix, aMacroVarNames, aVarDependenteNames, iQtdVarDependente);
		//print("mD_macro", mD_macro);
		savemat(sprint(txMatPathRawMatrix, sVarSufix, "R", iCont, "_D.mat"), mD_macro);

		// separo a matriz lambda em matriz lag-0 e demais lags
        mLambda_0 = mLambda[][0:iQtdVarDependente - 1];
        mLambda = mLambda[][iQtdVarDependente:];

        // Construção das matrizes A
        for (nContLags = 0; nContLags <= iQtdLags; ++nContLags) {
            if (nContLags == 0) {
                // Gero a matrix A_0
                A_0 = ( < 1, 0 > ** unit(iQtdVarDependente)) + ( < 0, -1 > ** mLambda_0);
                savemat(sprint(txMatPathA_Matrix, "A", iCont, "_", nContLags, ".mat"), A_0);
                //println(sprint(txMatPath, "A", iCont, "_", nContLags, ".mat"));
                //println(A_0);
            } else {
                A_L = ( < 1, 0 > ** mPhi[][(iQtdVarDependente * (nContLags - 1)):(((iQtdVarDependente * (nContLags - 1)) + iQtdVarDependente - 1))]) +
                      ( < 0, 1 > ** mLambda[][(iQtdVarDependente * (nContLags - 1)):(((iQtdVarDependente * (nContLags - 1)) + iQtdVarDependente - 1))]);
                savemat(sprint(txMatPathA_Matrix, "A", iCont, "_", nContLags, ".mat"), A_L);
                //println(sprint(txMatPathA_Matrix, "A", iCont, "_", nContLags, ".mat"));
                //println(A_L);
            }
        }
        // Apaga variaveis que nao usa mais.
        delete mPhi;
        delete mLambda;
        delete mLambda_0;
        delete mU;
        delete iContParam;
        delete iTotalParam;
        delete asParamNames;
        delete vParamValues;
        delete nContLags;
        delete A_0;
        delete A_L;
        // Faz os calculos para determinar as matrizes Wi
        decl Wi_aux1, Wi_aux2, Wi;
        Wi_aux1 = zeros(1, iQtdRegioes);
        Wi_aux1[][iCont - 1] = 1;
        Wi_aux1 = Wi_aux1 ** unit(iQtdVarDependente);
        Wi_aux2 = mW[][iCont - 1]';
        Wi_aux2 = Wi_aux2 ** unit(iQtdVarDependente);
        Wi = Wi_aux1 | Wi_aux2;
        //println("Wi", Wi);
        savemat(sprint(txMatPathW_Matrix, "W", iCont, ".mat"), Wi);
        delete Wi_aux1;
        delete Wi_aux2;
        delete Wi;
        println("\nApagando o modelo PcGive referente a regiao ", iCont);
        delete model;
    }
    delete mW, i;
    println("*** Fim da estimacao dos modelos regionais *** \n");
}
