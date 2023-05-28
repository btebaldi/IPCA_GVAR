#include <oxstd.oxh>
#import <packages/CATS/CATS>

// ##################################################################################################################################
//  CLASSES
// ##################################################################################################################################
class Cointegration {
    decl iRank;
	decl iLags;

	decl mRankMatrix;
		decl mRankMatrix2;
	decl mBeta;

	decl mX;
	decl asX;
	
	decl mX_star;
	decl asX_star;

	// Contructor 
	Cointegration();
	
	// Estima modelo de Cointegracao
	EstimateCoint();

	// Adiciona as variaveis X
	Add_X(const mData, const asNames);
	
	// Adiciona as variaveis X_star
	Add_X_star(const mData, const asNames);

	// Permite inicializar os lags com um valor especifico
	SetLags(const iValue);

	// Inicializa a contagem dos ranks
	InitRank();
	
	// Informacao do modelo
	Info();

	// Fixa Rank de cointegracao
	SetRank(const iValue);

	// Estima o Rank de cointegracao 
	EstimateRank();
	EstimateRankFromBoostrap();

	// Salva a estimativa do beta
	SaveBetaEstimative(const spath);

	GetBetaEstimative();
};
//Constructor class
Cointegration::Cointegration(){
	iLags = 2;
}
Cointegration::SetLags(const iValue){
	iLags = iValue;
}
// Metodo Add_X
Cointegration::Add_X(const mData, const asNames){
	mX = mData;
	asX = asNames;
}
// Metodo Add_X_Star
Cointegration::Add_X_star(const mData, const asNames){
	mX_star = mData;
	asX_star = asNames;
}
// Metodo inicializa o rank
Cointegration::InitRank(){
	iRank = columns(mX) + columns(mX_star);
}
// Seta o rank para valor especifico
Cointegration::SetRank(const iValue){
	iRank = iValue;
}

// Model Info
Cointegration::Info(){
	println("Rank: ", iRank);
	println("Lags: ", iLags);

	println("X Variables: ", asX);
	println("X* Variables: ", asX_star);
}

// Estimate Cointegration
Cointegration::EstimateCoint(){

	//--- Ox code for CATS( 8)
	decl model = new CATS();

	// Adiciona variaveis X ao banco de dados
	model.Append(mX, asX);

	// Adiciona variaveis X_star ao banco de dados
	model.Append(mX_star, asX_star);

	// Adiciona as variaveis X como exogenas
	for(decl iqtd = 0; iqtd < columns(asX); iqtd++) {
            println("append: ", asX[iqtd]);
			model.Select("Y", {asX[iqtd], 0, 0});
	}

	for(decl iqtd = 0; iqtd < columns(asX_star); iqtd++) {
            println("Star append: ", asX_star[iqtd]);
			model.Select("X", {asX_star[iqtd], 0, 0});
	}


	// Lags ( const lags , const lagsExo = - 1 , const lagsDet = - 1 )
	model.Lags(iLags, iLags, iLags);
	
	// Rank inicial (mudar para a quantidade de variaveis.)
	model.I1Rank(iRank);
	
	// Tipo de cointegracao CIMEAN: Constante no espaÃ§o de cointegracao.
    // mode	string: one of "NONE","CIMEAN","DRIFT","CIDRIFT".
    // Equivalently, use the strings "H_z","H_c","H_lc","H_l", or the predefined constants CATS::NONE, CATS::CIMEAN, CATS::DRIFT, CATS::CIDRIFT.
	model.Trend("DRIFT");

	// Inclui seasonal centradas
	model.Seasonals(1);

	// fixa a amostra
	// model.SetSelSample(1975, 9, 1998, 12);
	
	// tipo de metodo RRR: Reduced Rank Regression
	model.SetMethod("RRR");


	// set print to false
	model.SetPrint(TRUE);

	// Estima o modelo.
	model.Estimate();

	mRankMatrix2 = model.BootstrapRankTest();

	// Guarda o resultado o Rank table.
	mRankMatrix = model.I1RankTable();
	
	// Guarda o valor do Beta
	mBeta = model.GetBeta();

	delete model;
}
// Determina o rank de cointegracao pelo teste de rank
Cointegration::EstimateRank(){
	for(decl irow = 0; irow < rows(mRankMatrix); irow++) {
		iRank = mRankMatrix[irow][1];
		if(mRankMatrix[irow][6] > 0.05){
			break;
		} else {
			iRank = iRank + 1;
		}
	}
	println("Estimated rank: ", iRank);
}

Cointegration::EstimateRankFromBoostrap(){
	for(decl irow = 0; irow < rows(mRankMatrix2[0][][]); irow++) {
		// println("LINE[", irow, "]:", mRankMatrix2[0][irow][7]);
		iRank = mRankMatrix2[0][irow][1];
		if(mRankMatrix2[0][irow][7] > 0.05){
			break;
		} else {
			iRank = iRank + 1;
		}
	}
	println("Estimated rank: ", iRank);
}

Cointegration::SaveBetaEstimative(const spath){
	decl mbetaTransp = GetBetaEstimative();
	savemat(spath, mbetaTransp');
}

Cointegration::GetBetaEstimative(){
	iRank=0;
	decl ret;
	if (iRank == 0){
		ret = zeros(columns(mX) + columns(mX_star), 1);
	} else {
		ret = mBeta[][0:iRank-1];
	}
	return ret;
}