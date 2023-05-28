#include <oxstd.oxh>

main()
{
	// Arquivo de configuracao
	#include "./Config.ini"

	decl sVarSufix;
	sVarSufix = "";

	decl mWi, mAi_l, GL, mUi, mUStack, mAlphai, aAlphaiStack, mIISi, mIISiStack, mDi, mDStack,  iContRegion, iCurrentLag, mBetai;

	for(iCurrentLag = 0; iCurrentLag <= iQtdLags; ++iCurrentLag)
	{
	
		for(iContRegion=1; iContRegion<=iQtdRegioes; ++iContRegion)
	    {
			// Carrega a matriz de pesso da regiao.
			//println("Lendo matrix ", sprint(txMatPathW_Matrix, "W", iContRegion,".mat"));
			mWi = loadmat(sprint(txMatPathW_Matrix, "W", iContRegion,".mat"));
			
			// Carrega a Matriz de coeficientes
			//println("Lendo matrix ", sprint(txMatPathA_Matrix, "A", iContRegion, "_", iCurrentLag, ".mat"));
			mAi_l = loadmat(sprint(txMatPathA_Matrix, "A", iContRegion, "_", iCurrentLag, ".mat"));
			
			// primeira passagem, inicializo o G0 e GL
			if(iContRegion==1)
			{
				GL =  mAi_l * mWi;
			}
			else
			{
				GL = GL | (mAi_l * mWi);
			}
			//println(GL);
	    }
		println("Saving G", iCurrentLag, " matrix");
		savemat(sprint(txMatPathG_Matrix, "G", iCurrentLag, ".mat"), GL);
	}

	// PROCESSO DE CONSTRUCAO DA MATRIZ DE CONSTANTES E SEASONAL
	println("Realizando processo de stacking das matrizes U (Cosntantes e Seasons)");
	for(iContRegion=1; iContRegion<=iQtdRegioes; ++iContRegion)
	{
		//println("Lendo matrix ", sprint(txMatPathRawMatrix, sVarSufix, "R", iContRegion, "_U.mat"));
		mUi = loadmat(sprint(txMatPathRawMatrix, sVarSufix, "R", iContRegion, "_U.mat"));

		// primeira passagem, inicializo o U0 e UL
		if(iContRegion==1)
		{
			mUStack =  mUi;
		}
		else
		{
			mUStack = mUStack | mUi;
		}

		//println("Saving U_Stacked (region ", iContRegion, ")");
		savemat(sprint(txMatPathG_Matrix, "U_Stacked", ".mat"), mUStack);
	}


	// PROCESSO DE CONSTRUCAO DA MATRIZ ALPHA
	println("Realizando processo de stacking das matrizes Alpha");
	for(iContRegion=1; iContRegion<=iQtdRegioes; ++iContRegion)
	{
		//println("Lendo matrix ", sprint(txMatPathRawMatrix, sVarSufix, "R", iContRegion, "_Alpha.mat"));
		mAlphai = loadmat(sprint(txMatPathRawMatrix, sVarSufix, "R", iContRegion, "_Alpha.mat"));
		
		//mBetai = loadmat(sprint(txCoIntMatPath, "CoInt_R", iContRegion, ".mat"));

		//println("Lendo matrix ", sprint(txCoIntMatPath, "Weak2_CoInt_R", iContRegion, ".mat"));
		mBetai = loadmat(sprint(txCoIntMatPath, "Weak2_CoInt_R", iContRegion, ".mat"));

		//println("Lendo matrix ", sprint(txMatPathW_Matrix, "W", iContRegion, ".mat"));
		mWi = loadmat(sprint(txMatPathW_Matrix, "W", iContRegion, ".mat"));

		// Regularizacao do beta para operacionalizacao.
		if(columns(mAlphai) > rows(mBetai))
		{
		//adiciono uma linha zerada no beta pois nao há dois betas.
		mBetai = mBetai |reshape(0, columns(mAlphai) - rows(mBetai), columns(mBetai));;
		}

		// primeira passagem, inicializo o Aplha0 e AplhaL
		if(iContRegion==1)
		{
			aAlphaiStack =  mAlphai * mBetai * mWi;
		}
		else
		{
			//aAlphaiStack = (aAlphaiStack ~ zeros(rows(aAlphaiStack), columns(mAlphai))) | (zeros(rows(mAlphai), columns(aAlphaiStack)) ~ mAlphai);
			aAlphaiStack = aAlphaiStack | mAlphai * mBetai * mWi;
		}

		//println(sprint("Saving G_alpha (region ",iContRegion, ")"));
		savemat(sprint(txMatPathG_Matrix, "G_alpha", ".mat"), aAlphaiStack);
	}


	// PROCESSO DE CONSTRUCAO DA MATRIZ IIS
	println("Realizando processo de stacking das matrizes IIS");
	for(iContRegion=1; iContRegion<=iQtdRegioes; ++iContRegion)
	{
		mIISi = loadmat(sprint(txMatPathRawMatrix, sVarSufix, "R", iContRegion, "_IIS.mat"));

		// primeira passagem, inicializo o IIS0 e IISL
		if(iContRegion==1)
		{
			mIISiStack =  mIISi;
		}
		else
		{
			mIISiStack = mIISiStack | mIISi;
		}

		//println(sprint("Saving IIS (region ",iContRegion, ")"));
		savemat(sprint(txMatPathG_Matrix, "IIS_Stacked", ".mat"), mIISiStack);
	}

	// PROCESSO DE CONSTRUCAO DA MATRIZ D
	println("Realizando processo de stacking da matrizes D");
	for(iContRegion=1; iContRegion<=iQtdRegioes; ++iContRegion)
	{
		mDi = loadmat(sprint(txMatPathRawMatrix, sVarSufix, "R", iContRegion, "_D.mat"));

		// primeira passagem, inicializo o D0 e DL
		if(iContRegion==1)
		{
			mDStack =  mDi;
		}
		else
		{
			mDStack = mDStack | mDi;
		}

		//println(sprint("Saving D_Stacked (region ",iContRegion, ")"));
		savemat(sprint(txMatPathG_Matrix, "D_Stacked", ".mat"), mDStack);
	}

	println("Processo finalizado.");
}
