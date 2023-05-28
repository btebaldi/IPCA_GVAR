#include <oxstd.oxh>

main()
{
	// Arquivo de configuracao
	#include "./Config.ini"

	println("GVAR Passo 4 Inicializado");
	// enter code
	decl iQtdVarMacro;
	iQtdVarMacro = rows(aMacroVarNames); // Isso poderia ser melhorado???!!!
	
	decl mG0, mGy, mGy_inv, mGyL, mGL, iCurrentLag, mG_alpha, mIIS_Stacked, mU_Stacked, mIIS_w, mU_w,
	mD_Stacked, mD0_stk, mDL_stk, mC, mL, mHw, mHw_lag, mWw, mBw, mBWw, mAlphaBeta_w;

	// Carrega a matrix G0
	println("Carregando matriz G0");
	mG0 = loadmat(sprint(txMatPathG_Matrix, "G0.mat"));
	//println("mG0: ", mG0);

	// Carrega a matrix Stackeds
	println("Carregando matrizes stacked (Alpha, IIS, U, D)");
	mG_alpha = loadmat(sprint(txMatPathG_Matrix, "G_alpha.mat"));
	println("Matrix Alpha_Stacked.mat carregada");
	mIIS_Stacked = loadmat(sprint(txMatPathG_Matrix, "IIS_Stacked.mat"));
	println("Matrix IIS_Stacked.mat carregada");
	mU_Stacked = loadmat(sprint(txMatPathG_Matrix, "U_Stacked.mat"));
	println("Matrix U_Staked.mat carregada");
	mD_Stacked = loadmat(sprint(txMatPathG_Matrix, "D_Stacked.mat"));
  	println("Matrix D_Staked.mat carregada");


	mHw = loadmat(sprint(txMatPathRawMatrix, "H_w", ".mat"));
  	println("Matrix H_w.mat carregada");
	mBw = loadmat(sprint(txMatPathRawMatrix, "B_w", ".mat"));
  	println("Matrix B_w.mat carregada");
	mWw = loadmat(sprint(txMatPathW_Matrix, "W_w", ".mat"));
  	println("Matrix W_w.mat carregada");

	mIIS_w = loadmat(sprint(txMatPathRawMatrix, "IIS_w", ".mat"));
  	println("Matrix IIS_w.mat carregada");
	mU_w = loadmat(sprint(txMatPathRawMatrix, "U_w", ".mat"));
  	println("Matrix U_w.mat carregada");

	mAlphaBeta_w = loadmat(sprint(txMatPathRawMatrix, "AlphaBeta_w", ".mat"));
  	println("Matrix AlphaBeta_w.mat carregada");
	
	//Separacao da matriz D_stk em D0_stk e DL_stk
	mD0_stk = mD_Stacked[0:][0:iQtdVarMacro-1];
	//println("mD0_stk: ", mD0_stk);

	mGy = (unit(iQtdVarMacro) | -mD0_stk)~(zeros(iQtdVarMacro,columns(mG0)) | mG0);
	// println(sprint("mGy:", rows(mGy), "x", columns(mGy)));
	
	savemat(sprint(txMatPathResult_Matrix, "mGy", ".mat"), mGy);
	mGy_inv = invert(mGy);
	
	// Salva as matrizes de resultado
	println("Salvando matrizes stacked (Alpha, IIS, U, D)");


	for(iCurrentLag = 1; iCurrentLag <= iQtdLags; ++iCurrentLag)
	{
		// Carrega a matrix de constantes
		println("Carregando matrizes stacked (G", iCurrentLag,")");
		mGL = loadmat(sprint(txMatPathG_Matrix, "G", iCurrentLag, ".mat"));
		//println("mGL (lag:", iCurrentLag,"): ", mGL);

		println("iQtdVarMacro ",  iQtdVarMacro);

		println("Costruindo matriz D_{stk; l} (lag:", iCurrentLag,")");
		mDL_stk	= mD_Stacked[][iQtdVarMacro*iCurrentLag:(iQtdVarMacro*(iCurrentLag+1))-1];

		//println(sprint("mDL_stk", "(", iCurrentLag,")", ": "), mDL_stk);

		//println("mHw", mDL_stk);
		mHw_lag	= mHw[][(iQtdVarMacro*(iCurrentLag-1)):((iQtdVarMacro*(iCurrentLag))-1)];
		//println(sprint("mHw_lag", "(", iCurrentLag,")", ": "), mHw_lag);



		mBWw = mBw[][iCurrentLag-1] * mWw;

		println("Size mHw_lag:", sizer(mHw_lag), " x ", sizec(mHw_lag));
		println("Size mDL_stk:", sizer(mDL_stk), " x ", sizec(mDL_stk));
		println("Size mBWw:", sizer(mBWw), " x ", sizec(mBWw));
		println("Size mGL:", sizer(mGL), " x ", sizec(mGL));
		println("Size mGy_inv:", sizer(mGy_inv), " x ", sizec(mGy_inv));
		
		
		mGyL = (mHw_lag | mDL_stk) ~ (mBWw | mGL);
		//println("mGyL:", mGyL);
		// println(sprint("mWw:", rows(mWw), "x", columns(mWw)));

		println("Salvando o resultado mGy_inv * mGyL");
		savemat(sprint(txMatPathResult_Matrix, "mGy_inv_X_mGyL",iCurrentLag,".mat"), (mGy_inv * mGyL));
		//println("mDL_stk:", mDL_stk[][iQtdVarMacro*(iCurrentLag-1):(iQtdVarMacro*(iCurrentLag))-1]);
	}
																								  


	println("Size mU_w:", sizer(mU_w), " x ", sizec(mU_w));
	println("Size mU_Stacked:", sizer(mU_Stacked), " x ", sizec(mU_Stacked));
	println("Size mIIS_w:", sizer(mIIS_w), " x ", sizec(mIIS_w));
	println("Size mIIS_Stacked:", sizer(mIIS_Stacked), " x ", sizec(mIIS_Stacked));
	//println("Size mGy_inv:", sizer(mGy_inv), " x ", sizec(mGy_inv));
	

	mC = (mU_w | mU_Stacked) ~(mIIS_w | mIIS_Stacked);
	println("Salvando o resultado mGy_inv * mC");
	savemat(sprint(txMatPathResult_Matrix, "mGy_inv_X_mC", ".mat"), (mGy_inv * mC));

//	mAlphaBeta_w  mG_alpha
	mL = (mAlphaBeta_w | zeros(rows(mG_alpha), columns(mAlphaBeta_w)) );
	mL ~= (zeros(rows(mAlphaBeta_w), columns(mG_alpha)) | mG_alpha);
	println("Salvando o resultado mGy_inv * mL");
	savemat(sprint(txMatPathResult_Matrix, "mGy_inv_X_mL", ".mat"), (mGy_inv * mL));
	

//	savemat(sprint(txMatPathResult_Matrix, "Result_Alpha.mat"), (mG0_inv * mAlpha_Stacked));
//	savemat(sprint(txMatPathResult_Matrix, "Result_IIS.mat"), (mG0_inv * mIIS_Stacked));
//	savemat(sprint(txMatPathResult_Matrix, "Result_U.mat"), (mG0_inv * mU_Stacked));
//	savemat(sprint(txMatPathResult_Matrix, "Result_D.mat"), (mG0_inv * mD_Stacked));



	println("***************************************************");
	println("GVAR Passo 4 Finalizado");
}
