// ������������� ����������� ���������
#include <iostream>
#include <fstream>
#include <string>
#include <chrono>
#include <tuple>
#include <cmath>
#include <complex>
#include <Eigen/Dense>
#include <Eigen/LU>
#include <OpenXLSX.hpp>

using namespace std;
using namespace std::chrono;

using namespace OpenXLSX;
using namespace Eigen;

std::ofstream debug_file;

// ������ ������� ��� ���������� ������ �� �������
void insert_gap()
{
	debug_file << ' ' << endl;
}

void insert_start_separator()
{
	debug_file << " " << endl;
	debug_file << "======================== *START* ========================" << endl;
	debug_file << " " << endl;
}

void insert_end_separator()
{
	debug_file << " " << endl;
	debug_file << "========================= *END* =========================" << endl;
	debug_file << " " << endl;
}


// ������� ��� ����������� ������ � ����� � ����. ������
float stringToFloat(string s)
{
	float toFloat;
	stringstream converter(s);
	converter >> toFloat;
	return toFloat;
}

// ������� ��� ����������� ������ � �����
int stringToInt(string s)
{
	int toInt;
	stringstream converter(s);
	converter >> toInt;
	return toInt;
}


// ����� �������� ��� ��������� ������ �� ��������� � ���� ��������
class CustomRangePairs
{
public:
	int difference = -1;

	tuple<int, int> get_range_pairs(int harm)
	{
		tuple <int, int> two_pairs;

		int phase = harm + difference;
		int amplitude = phase++;

		two_pairs = make_tuple(amplitude, phase);
		difference++;
		return two_pairs;
	}

	void reset()
	{
		difference = -1;
	}
};

// ����� �������� ��� ��������� ������ �� ����� ����
class PhaseSheetsHandler
{
public:
	int odd_difference = -1;

	int get_phase_number(int sheet_number)
	{
		int phase_number = sheet_number + odd_difference;
		odd_difference--;
		return phase_number;
	}

	void reset()
	{
		odd_difference = -1;
	}
};


// ����� �������� ��� ����������� ��������� ��� ������������� �������������
class ProsoedRowRange 
{
public:
	tuple<int, int> get_range(int which, int titles_indexes[], int num_pris)
	{
		// which = [1-6] ���� 6 ������������� � �������.
		tuple <int, int> pris_range;
		int r_start = titles_indexes[which - 1] + 1;
		int r_end = titles_indexes[which] - 1;
		
		if (which == num_pris) r_end++;
		pris_range = make_tuple(r_start, r_end);
		
		return pris_range;
	}
};

// ���������� �������� ���������� �������
const int num_pris = 6; // ����� ���������� ������������� � ������� ���������� 
const int pris_num = 1; // 1 - ��� ������, 2 - ��� ������, 3 - �������, .. (��������� ������)
const int num_phases = 3;
const int num_tross = 1;
const int num_harms = 49;
const int num_recs = 560; // ~ ���������� ��������� � ��������� ��� ������� �������������
int titles_indexes[(num_pris+1)];
int sheets_counter = 1;
int titles_counter = 0;
int rows_counter = 0;
int phase_number;

// ������� ������. ��������� ������
double UM[3][50][700] = {0};  double AIM[3][50][700] = {0};
double FUM[3][50][700] = {0}; double FIM[3][50][700] = {0};

double UM1[3][50][700] = {0}; double UM2[3][50][700] = {0};
double AIM1[3][50][700] = {0}; double AIM2[3][50][700] = {0};

double PPR1[700] = {0}; double PPR2[700] = {0};

double PD[3][700] = {0}; double PPP[50][1000] = {0};
double PPP1[50][1000] = {0}; double PPP5[50][1000] = {0};
double PPP2[50][1000] = {0}; double PPP6[50][1000] = {0};
double PPP3[50][1000] = {0}; double PPP7[50][1000] = {0};
double PPP4[50][1000] = {0}; double PPP8[50][1000] = {0};

double WD[2][50] = {0}; double II[10] = {0};

enum main_harm_enum { knsu_e = 0, knsi_e, rmsu_e, rmsi_e, funu_e, funi_e, fu_e, fi_e };
double main_harm[8][3][700] = {0};


// ������ � ����������� �����
const int IH[16] = { 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0 };

VectorXcd UK1;
VectorXcd AIK1;

std::complex<double> UK10(0, 0); std::complex<double> AIK10(0, 0);
std::complex<double> UK11(0, 0); std::complex<double> AIK11(0, 0);
std::complex<double> UK12(0, 0); std::complex<double> AIK12(0, 0);
std::complex<double> AL(-0.5, 0.866025);

double SKU0 = 0, SKU2 = 0, SKI0 = 0, SKI2 = 0;

const double PI = 3.14159265358979;

double RZ = 35.3;
double FF = 0, SS2 = 0, SS0 = 0, PP1 = 0, PP2 = 0, RPR = 0, PRP = 0, WD0 = 0, WD1 = 0, WD4 = 0, WD10 = 0;

std::complex<double> SS(0., 0.), SS1(0., 0.);

// ����� ���������� ��� ��������� �������! [MM, M, M1, MT, M10, M20, PR, K1, K2, K3, N1, N2, N3, MPR, MTR, MMT]
// ������ � ������������ ����
int M = 0, M1 = 0,  M10 = 0, M20 = 0, PR = 0, K1 = 0, K2 = 0, K3 = 0, N1 = 0, N2 = 0, N3 = 0, MMT = 0;

const int MM = 5;
const int MPR = 3;
const int MTR = 1;
const double DT = 2.5;
const int MT = 5;

// ������� ���������� ��� � ������ ����� 
const double XA[num_phases+num_tross] = {0.0, 6.3, 4.2, 2.1};
const double YA[num_phases + num_tross] = {19.0, 19.0, 25.0, 28.0};
const double OMP[num_phases + num_tross] = {1.0, 1.0, 1.0, 4000.0};
const double GM[num_phases + num_tross] = {35.336, 35.336, 35.336, 17.336};
const double S[num_phases + num_tross] = {150.0, 150.0, 150.0, 50.0};


// ��������� ������� ���������!
void raschet(int& k, int& n)
{
	// !!! �������� !!!        k(� �++) = LL(� ��������) = [0-49]     and      n(� �++) = NN(� ��������) = [0-559]	

	// ��������� ������� ���� ��� �������� �������� � ������ �����. �� ���������� ��������� �� ������.
	// ��������� ���������� ����������� ��� ������������� �������� (���������) ���������� ������ ������� �������.
	const int M = ::MPR + ::MTR;
	const int MMT = ::MM / ::MT;
	const int M10 = 2 * M;
	const int M20 = 4 * M;

	int M1;
	if (M <= 6) M1 = M;
	if (M > 6) M1 = 6;


	//  ��� ������� (����) ����������� (���������) ����� ��������� ��������� ��������

	MatrixXcd HH11; HH11 = MatrixXcd::Zero(M, M);
	MatrixXcd HH21; HH21 = MatrixXcd::Zero(M, M);
	MatrixXcd HH13; HH13 = MatrixXcd::Zero(M, M);
	MatrixXcd HH23; HH23 = MatrixXcd::Zero(M, M);
	MatrixXcd HH32; HH32 = MatrixXcd::Zero(M, M);
	MatrixXcd HH42; HH42 = MatrixXcd::Zero(M, M);
	MatrixXcd HH34; HH34 = MatrixXcd::Zero(M, M);
	MatrixXcd HH44; HH44 = MatrixXcd::Zero(M, M);

	VectorXd DET10; VectorXd DET20; // VectorXd SS; VectorXd SS1;
	VectorXcd DET1; DET1 = VectorXcd::Zero(M);
	VectorXd DET2; DET2 = VectorXd::Zero(M);

	MatrixXcd GG;  GG = MatrixXcd::Zero(M20, M20);
	MatrixXcd GG1; GG1 = MatrixXcd::Zero(M20, M20);
	MatrixXcd GG2; GG2 = MatrixXcd::Zero(M20, M20);
	MatrixXcd GG4; GG4 = MatrixXcd::Zero(M10, M10);
	MatrixXcd GG5; GG5 = MatrixXcd::Zero(M10, M10);

	MatrixXcd F; F = MatrixXcd::Zero(M, M);
	MatrixXd F2; F2 = MatrixXd::Zero(M, M);
	MatrixXd F3; F3 = MatrixXd::Zero(M, M);
	MatrixXd F4; F4 = MatrixXd::Zero(M, M);
	MatrixXd F5; F5 = MatrixXd::Zero(M, M);
	MatrixXd F6; F6 = MatrixXd::Zero(M, M);
	MatrixXd F7; F7 = MatrixXd::Zero(M, M);
	MatrixXd F10; F10 = MatrixXd::Zero(M, M);

	VectorXd B; B = VectorXd::Zero(M);
	VectorXcd B1; B1 = VectorXcd::Zero(M20);
	VectorXd B4; B4 = VectorXd::Zero(M20);
	VectorXd B6; B6 = VectorXd::Zero(M10);
	VectorXd B7; B7 = VectorXd::Zero(M10);
	VectorXcd B10; B10 = VectorXcd::Zero(M10);

	MatrixXcd LU;  LU = MatrixXcd::Zero(M, M);
	MatrixXcd LU1; LU1 = MatrixXcd::Zero(M, M);
	MatrixXcd LU2; LU2 = MatrixXcd::Zero(M, M);
	MatrixXcd LU3; LU3 = MatrixXcd::Zero(M, M);
	MatrixXcd LI;  LI = MatrixXcd::Zero(M, M);
	MatrixXcd LI1; LI1 = MatrixXcd::Zero(M, M);
	MatrixXcd LI2; LI2 = MatrixXcd::Zero(M, M);
	MatrixXcd LI3; LI3 = MatrixXcd::Zero(M, M);

	MatrixXcd Z; Z = MatrixXcd::Zero(M, M);
	MatrixXcd Y; Y = MatrixXcd::Zero(M, M);

	VectorXd AA; AA = VectorXd::Zero(M);
	VectorXd BB; BB = VectorXd::Zero(M);
	MatrixXd CC; CC = MatrixXd::Zero(M, M);
	MatrixXd DD; DD = MatrixXd::Zero(M, M);

	MatrixXd HC1; HC1 = MatrixXd::Zero(M, M);
	MatrixXd HC3; HC3 = MatrixXd::Zero(M, M);

	VectorXd EVI; EVI = VectorXd::Zero(M);
	// VectorXd EVU; EVU = VectorXd::Zero(M);

	// MatrixXd AU; AU = MatrixXd::Zero(M, M);
	MatrixXd AAI; AAI = MatrixXd::Zero(M, M);



	// ���������� �������� (���������) ���������� ������ ������� �������.
	double R0[M] = { 0 }, R[M] = { 0 }, UXM[M] = { 0 }, HI[M] = { 0 }, R11[M] = { 0 }, DET4[M] = { 0 },
		AIXM[M] = { 0 };
	std::complex<double> UX[M] = { 0 }, AIX[M] = { 0 }, SM[M] = { 0 }, DET3[M] = { 0 };

	std::complex<double> B5[M20] = { 0 };

	double HC2[M][M] = { 0 }, HC4[M][M] = { 0 },
		XL[M][M] = { 0 }, XL1[M][M] = { 0 }, G[M][M] = { 0 }, D[M][M] = { 0 }, HC[M][M] = { 0 };

	std::complex<double> E[M][M] = { 0 }, F1[M][M] = { 0 };

	double D1[M][M] = { 0 }, D2[M][M] = { 0 }, D3[M][M] = { 0 };

	double HH[M10][M10] = { 0 };
	std::complex<double> HH12[M][M] = { 0 }, HH14[M][M] = { 0 },
		HH22[M][M] = { 0 }, HH24[M][M] = { 0 },
		HH31[M][M] = { 0 }, HH33[M][M] = { 0 },
		HH41[M][M] = { 0 }, HH43[M][M] = { 0 };

	std::complex<double> GG3[M10][M20] = { 0 };


	// ������� �������� ������������ �������� ������� � ������� ���������� Eigen 
	MatrixXcd A1; A1 = MatrixXcd::Zero(M1, M1);
	MatrixXcd A2; A2 = MatrixXcd::Zero(M1, M1);
	VectorXd IPVT1; IPVT1 = VectorXd::Zero(M1);

	// ������� �������� � �������� AG. 3-x ������ [M1][M][M]. �1 - ���������!!!
	// �������� ������ � �������� �1, �������� � ���� ��� ���������� � �������� ���
	std::vector<MatrixXd> AG;
	for (int i = 0; i < M1; i++)
	{
		MatrixXd ag; ag = MatrixXd::Zero(M, M);
		AG.push_back(ag);
	}

	if (PR == 1) PP1 = 0;
	if (PR == 2) PP2 = 0;

	float W = k + 1;
	std::complex<double> EX1(2.71828, 0.);

	// ������ � ���� #5 "��������� ����� ������" (���������� ���������!)

	// ���� #845
	for (int i = 0; i < M; i++)
	{
		R[i] = sqrt(S[i] / PI) / 1000.;
		HI[i] = R[i] / (2.) * sqrt(2. * PI * W * 50. * 4. * PI * OMP[i] * GM[i] / 20.);
		R0[i] = 1000. / (GM[i] * S[i]);
		if (HI[i] < 1) R11[i] = R0[i] * (1 + std::pow(HI[i], 4. / 3.));
		if (HI[i] > 1) R11[i] = R0[i] * (HI[i] + 0.25 + 3. / (64. * HI[i]));
		if (i == M);
	}

	// ���� #12. �� ����� ���� ������!
	for (int i = 0; i < M10; i++)
	{
		for (int j = 0; j < M10; j++)
		{
			HH[i][j] = 0;
		}
	}

	// ���� #161
	for (int i = 0; i < M; i++)
	{
		for (int j = 0; j < M; j++)
		{
			if (i == j) D[i][i] = R[i];
			if (i != j) D[i][j] = sqrt(pow((XA[i] - XA[j]), 2) + pow((YA[i] - YA[j]), 2));
			HC[i][j] = sqrt(pow((XA[i] - XA[j]), 2) + pow((YA[i] + YA[j]), 2));
			E[i][j] = std::complex(0.0, 0.0);
			E[i][i] = std::complex(1.0, 0.0);
		}
	}

	// ���� #740
	for (int i = 0; i < M; i++)
	{
		for (int j = 0; j < M; j++)
		{
			XL1[i][j] = (0.145 * log10(1000. / D[i][j])) / (100. * PI);
		}
	}

	// ���� #743
	for (int i = 0; i < M; i++)
	{
		for (int j = 0; j < M; j++)
		{
			HC1(i, j) = 41.4 * pow(10., 6.) * log10(HC[i][j] / D[i][j]);
		}
	}

	HC3 = HC1.inverse();
	F10 = HC1 * HC3;

	// ���� #744
	for (int i = 0; i < M; i++)
	{
		for (int j = 0; j < M; j++)
		{
			HC2[i][j] = HC3(i, j) * 2. * PI * 50.;
		}
	}

	// ���� #847
	for (int i = 0; i < M; i++)
	{
		for (int j = 0; j < M; j++)
		{
			XL[i][j] = XL1[i][j] * W * 2. * 50. * PI;
			HC4[i][j] = HC2[i][j] * W;
			double R10 = 0.0;
			if (i == j) Z(i, j) = std::complex<double>(R11[i], XL[i][j]);
			if (i != j) Z(i, j) = std::complex<double>(R10, XL[i][j]);
			if (i == j) G[i][j] = 0.00000004 * YA[i] / YA[i];
			if (i != j) G[i][j] = -0.00000004 * YA[0] / D[i][j];
			G[i][j] = 0.;
			Y(i, j) = complex(G[i][j], HC4[i][j]);
		}
	}


	// ���� #1300. �� ����� ������ ��������� �������!
	for (int iii = 0; iii < ::MT; iii++)
	{
		if (M != 3) goto label_767;
		// ���� #761
		for (int i = 0; i < 3; i++)
		{
			B5[i] = UK1(i);
			B5[i + 3] = AIK1(i);
			B5[i + 6] = std::complex<double>(0., 0.);
			B5[i + 9] = std::complex<double>(0., 0.);
		}
	label_767:
		if (M != 4) goto label_768;
		// ���� #762
		for (int i = 0; i < 3; i++)
		{
			B5[i] = UK1(i);
			B5[M - 1] = std::complex<double>(0., 0.);
			B5[i + M] = AIK1(i);
			B5[2 * M - 1] = std::complex<double>(0., 0.);
			B5[i + 2 * M] = std::complex<double>(0., 0.);
			B5[3 * M - 1] = std::complex<double>(0., 0.);
			B5[i + 3 * M] = std::complex<double>(0., 0.);
			B5[4 * M - 1] = std::complex<double>(0., 0.);
		}
	label_768:
		if (M != 6) goto label_769;
		// ���� #764
		for (int i = 0; i < 3; i++)
		{
			B5[i] = UK1(i);
			B5[i + 3] = UK1(i);
			B5[i + M] = AIK1(i);
			B5[i + M + 3] = AIK1(i);
			B5[i + 2 * M] = std::complex<double>(0., 0.);
			B5[i + 2 * M + 3] = std::complex<double>(0., 0.);
			B5[i + 3 * M] = std::complex<double>(0., 0.);
			B5[i + 3 * M + 3] = std::complex<double>(0., 0.);
		}
	label_769:
		if (M != 7) goto label_770;
		// ���� #765
		for (int i = 0; i < 3; i++)
		{
			B5[i] = UK1(i);
			B5[i + 3] = UK1(i);
			B5[M - 1] = std::complex<double>(0., 0.);
			B5[i + M] = AIK1(i);
			B5[i + M + 3] = AIK1(i);
			B5[2 * M - 1] = std::complex<double>(0., 0.);
			B5[i + 2 * M] = std::complex<double>(0., 0.);
			B5[i + 2 * M + 3] = std::complex<double>(0., 0.);
			B5[3 * M - 1] = std::complex<double>(0., 0.);
			B5[i + 3 * M] = std::complex<double>(0., 0.);
			B5[i + 3 * M + 3] = std::complex<double>(0., 0.);
			B5[4 * M - 1] = std::complex<double>(0., 0.);
		}

	label_770:
		if (M != 8) goto label_771;
		// ���� #766
		for (int i = 0; i < 3; i++)
		{
			B5[i] = UK1(i);
			B5[i + 3] = UK1(i);
			B5[MPR + 1 - 1] = std::complex<double>(0., 0.);
			B5[MPR + 2 - 1] = std::complex<double>(0., 0.);
			B5[i + M] = AIK1(i);
			B5[i + M + 3] = AIK1(i);
			B5[2 * M - 1 - 1] = std::complex<double>(0., 0.);
			B5[2 * M - 1] = std::complex<double>(0., 0.);
			B5[i + 2 * M] = std::complex<double>(0., 0.);
			B5[i + 2 * M + 3] = std::complex<double>(0., 0.);
			B5[3 * M - 1 - 1] = std::complex<double>(0., 0.);
			B5[3 * M - 1] = std::complex<double>(0., 0.);
			B5[i + 3 * M] = std::complex<double>(0., 0.);
			B5[i + 3 * M + 3] = std::complex<double>(0., 0.);
			B5[4 * M - 1 - 1] = std::complex<double>(0., 0.);
			B5[4 * M - 1] = std::complex<double>(0., 0.);
		}

	label_771:
		// *********************************************
		// ���������� ������� LU
		// ������������ ������ ����������
		// �����! AU ���� ������������ !!!
		auto AU = Z * Y;
		SS1 = sqrt(AU(0, 0));

		// ���������� ����������� �������� ������� �U
		// �����! EVU ���� ������������ !!!
		auto EVU = AU.inverse();

		// ������������ ������� �����������
		// ���� #20
		for (int j = 0; j < M; j++)
		{
			for (int i = 0; i < M; i++)
			{
				F(i, j) = pow(EVU(i), j);
				F1[i][j] = F(i, j);
			}
		}

		// ������������ ������� �����������
		// ���� #201
		for (int i = 0; i < M1; i++)
		{
			for (int j = 0; j < M1; j++)
			{
				A1(i, j) = F(i, j);
			}
		}
	}
}

// ������� ������� ������� ���������!
int main() {
	
	auto start = high_resolution_clock::now();

	debug_file.open("debug.txt", std::ios_base::app);

	XLDocument doc;
	doc.open("./Promzona.xlsx"); // ��������� Excel ����
	auto workbook = doc.workbook();
	auto check_worksheet = workbook.worksheet(workbook.worksheetNames()[0]);

	int worksheets_count = doc.workbook().worksheetCount();
	int f_columns_count = check_worksheet.columnCount();
	int f_rows_count = check_worksheet.rowCount();

	// ����� �������� ������ � Excel �����
	insert_start_separator();
	debug_file << "Workbook's WorkSheets count: " << worksheets_count << endl;

	insert_gap();
	debug_file << "First Worksheet's Columns count: " << f_columns_count << endl;
	debug_file << "First Worksheet's Rows count: " << f_rows_count << endl;
	insert_gap();

	// ����������� ����� � ������ ������ ��������� ��� ������� ������������� � �����
	titles_counter = 0;
	for (auto& row : check_worksheet.rows())
	{
		for (auto& cell_value : row.cells(1))
		{
			if (cell_value.value().typeAsString() == "string")
			{
				titles_indexes[titles_counter] = cell_value.cellReference().row();
				titles_counter++;
			}
		}
	}
	titles_indexes[num_pris] = f_rows_count;

	PhaseSheetsHandler phaser;
	CustomRangePairs ranger;
	ProsoedRowRange prisoeder;

	const auto [first, second] = prisoeder.get_range(pris_num, titles_indexes, num_pris);

	// ������ EXCEL ����� � ������ ���������� ������
	for (auto& worksheet_name : workbook.worksheetNames())
	{
		// ��� ������ ����� � ��������� ... ["Sheet1", "Sheet2", "Sheet3", "Sheet4", "Sheet5", "Sheet6"]
		auto worksheet = workbook.worksheet(worksheet_name);
		int w_columns_count = worksheet.columnCount();
		int w_rows_count = worksheet.rowCount();

		if (!(sheets_counter % 2 == 0)) { phase_number = phaser.get_phase_number(sheets_counter); }
		else { phase_number = (sheets_counter - 2) / 2; };

		// � ����������� �� ��������������� �������������...
		for (auto& row : worksheet.rows(first, second)) // ��� ������ ������ ������� �������������
		{

			std::deque<XLCellValue> cell(row.values()); // ����� ���� ����� � ������ ������

			if (sheets_counter % 2 == 0)                        // ������ ����� [Sheet2, Sheet4, Sheet6]
			{
				// ��������� � ���� ��� ����
				for (int h = 1; h < ((w_columns_count - (w_columns_count - (num_harms * 2))) / 2) + 1; h++) // h = [1-49]
				{
					const auto [amp, pha] = ranger.get_range_pairs(h);
					try { AIM[phase_number][h][rows_counter] = cell.at(amp).get<double>(); }
					catch (XLValueTypeError) { AIM[phase_number][h][rows_counter] = (double)cell.at(amp).get<int>(); }
					try { FIM[phase_number][h][rows_counter] = cell.at(pha).get<double>(); }
					catch (XLValueTypeError) { FIM[phase_number][h][rows_counter] = (double)cell.at(pha).get<int>(); }
				}

				// ���������� (���������) 8 ��������. ������ �������� ���������
				for (int i = 0; i < w_columns_count - (num_harms * 2); i++)
				{
					try { main_harm[i][phase_number][rows_counter] = cell.at(num_harms * 2 + i).get<double>(); }
					catch (XLValueTypeError) { main_harm[i][phase_number][rows_counter] = (double)cell.at(num_harms * 2 + i).get<int>(); }
				}

				ranger.reset();
			}
			else                                                // �������� ����� [Sheet1, Sheet3, Sheet5]
			{
				// ��������� � ���� ��� ���������� 
				for (int h = 1; h < (w_columns_count / 2) + 1; h++) // h = [1-49]
				{
					const auto [amp, pha] = ranger.get_range_pairs(h);
					try { UM[phase_number][h][rows_counter] = cell.at(amp).get<double>(); }
					catch (XLValueTypeError) { UM[phase_number][h][rows_counter] = (double)cell.at(amp).get<int>(); }
					try { FUM[phase_number][h][rows_counter] = cell.at(pha).get<double>(); }
					catch (XLValueTypeError) { FUM[phase_number][h][rows_counter] = (double)cell.at(pha).get<int>(); }
				}
				ranger.reset();
			}
			rows_counter++;
		}
		rows_counter = 0;
		sheets_counter++;
	}
	phaser.reset();
	sheets_counter = 1;

	// ����� EXCEL ����� ���������. ��������������� ������� ����������.
	// ���������� ��������!

	// ����������� ����������� ��� ������� �������� ���������
	main_harm_enum knsu, knsi, rmsu, rmsi, funu, funi, fu, fi;
	knsu = knsu_e; rmsu = rmsu_e; funu = funu_e; fu = fu_e;
	knsi = knsi_e; rmsi = rmsi_e; funi = funi_e; fi = fi_e;


	// ���� #13 � ��������
	for (int r = 0; r < num_recs; r++)
	{
		for (int p = 0; p < num_phases; p++)
		{
			UM[p][0][r] = main_harm[funu][0][r] * sqrt(2.);
			FUM[p][0][r] = main_harm[fu][0][r] * (PI / 180.);
			UM1[p][0][r] = UM[p][0][r] * cos(FUM[p][0][r]);
			UM2[p][0][r] = UM[p][0][r] * sin(FUM[p][0][r]);
		};
	};
	// ���� #14 � ��������
	for (int r = 0; r < num_recs; r++)
	{
		for (int p = 0; p < num_phases; p++)
		{
			for (int h = 1; h < num_harms; h++)
			{
				UM[p][h][r] = UM[p][h][r] * UM[p][0][r] / 100.;
				FUM[p][h][r] = FUM[p][h][r] * PI / 180.;
				UM1[p][h][r] = UM[p][h][r] * cos(FUM[p][h][r]);
				UM2[p][h][r] = UM[p][h][r] * sin(FUM[p][h][r]);
			}
		}
	}
	// ���� #16 � ��������
	for (int r = 0; r < num_recs; r++)
	{
		for (int p = 0; p < num_phases; p++)
		{
			AIM[p][0][r] = main_harm[funi][p][r] * sqrt(2.);
			FIM[p][0][r] = main_harm[fi][p][r] * PI / 180.;
			AIM1[p][0][r] = AIM[p][0][r] * cos(FIM[p][0][r]);
			AIM2[p][0][r] = AIM[p][0][r] * sin(FIM[p][0][r]);
		}
	}
	// ���� #117 � ��������
	for (int r = 0; r < num_recs; r++)
	{
		for (int p = 0; p < num_phases; p++)
		{
			for (int h = 1; h < num_harms; h++)
			{
				AIM[p][h][r] = AIM[p][h][r] * AIM[p][0][r] / 100.;
				FIM[p][h][r] = FIM[p][h][r] * PI / 180.;
				AIM1[p][h][r] = AIM[p][h][r] * cos(FIM[p][h][r]);
				AIM2[p][h][r] = AIM[p][h][r] * sin(FIM[p][h][r]);
			}
		}
	}


	// ���� #1500. �������!
	for (int n = 0; n < num_recs; n++) // 560 ������
	{
		for (int k = 0; k < num_harms; k++) // 50 ������
		{
			//label_1500:
			PR = 0;
		label_1700:
			PR = PR + 1;

			UK1[0] = std::complex<double>(UM1[0][k][n], UM2[0][k][n]);
			UK1[1] = std::complex<double>(UM1[1][k][n], UM2[1][k][n]);
			UK1[2] = std::complex<double>(UM1[2][k][n], UM2[2][k][n]);
			UK1[3] = std::complex<double>(0.0, 0.0);

			AIK1[0] = std::complex<double>(AIM1[0][k][n], AIM2[0][k][n]);
			AIK1[1] = std::complex<double>(AIM1[1][k][n], AIM2[1][k][n]);
			AIK1[2] = std::complex<double>(AIM1[2][k][n], AIM2[2][k][n]);
			AIK1[3] = std::complex<double>(0.0, 0.0);

			if (k > 1) goto label_1111;
			if (k == 1 && PR == 2) goto label_1111;

			UK10 = (UK1[0] + UK1[1] + UK1[2]) / 3.;
			UK11 = (UK1[0] + UK1[1] * AL + UK1[2] * std::pow(AL, 2.)) / 3.;
			UK12 = (UK1[0] + UK1[1] * std::pow(AL, 2.) + UK1[2] * AL) / 3.;

			// ��������� 2 ������ �� ����� ������� ������������ ������ � ���������, �� ���� � ���� ��������! ����� �������.
			SKU2 = sqrt(std::pow(real(UK12), 2.) + std::pow(imag(UK12), 2.)) / sqrt(std::pow(real(UK11), 2.) + std::pow(imag(UK11), 2.)) * 100.;
			SKU0 = sqrt(std::pow(real(UK10), 2.) + std::pow(imag(UK10), 2.)) / sqrt(std::pow(real(UK11), 2.) + std::pow(imag(UK11), 2.)) * 100.;

			UK1[0] = UK11;
			UK1[1] = UK11 * std::pow(AL, 2.);
			UK1[2] = UK11 * AL;

			AIK10 = (AIK1[0] + AIK1[1] + AIK1[2]) / 3.;
			AIK11 = (AIK1[0] + AIK1[1] * AL + AIK1[2] * std::pow(AL, 2.)) / 3.;
			AIK12 = (AIK1[0] + AIK1[1] * std::pow(AL, 2.) + AIK1[2] * AL) / 3.;

			// ��������� 2 ������ �� ����� ������� ������������ ������ � ���������, �� ���� � ���� ��������! ����� �������.
			SKI2 = sqrt(std::pow(real(AIK12), 2.) + std::pow(imag(AIK12), 2.)) / sqrt(std::pow(real(AIK11), 2.) + std::pow(imag(AIK11), 2.)) * 100.;
			SKI0 = sqrt(std::pow(real(AIK10), 2.) + std::pow(imag(AIK10), 2.)) / sqrt(std::pow(real(AIK11), 2.) + std::pow(imag(AIK11), 2.)) * 100.;

			AIK1[0] = AIK11;
			AIK1[1] = AIK11 * std::pow(AL, 2.);
			AIK1[2] = AIK11 * AL;

		label_1111:

			// ����� ��������� �������!
			raschet(k, n);

			if (k == 1 && PR == 1) PPR1[n] = PP1;
			if (k == 1 && PR == 2) PPR2[n] = PP2;
			if (k == 1 && PR == 1) goto label_1700;
			if (PR == 2) continue; //goto label_1500;
		}
	}


	// ���� #1501
	for (int r = 0; r < num_recs; r++) 
	{
		PRP = 0;
		// ���� #1060
		for (int h = 0; h < num_harms; h++) 
		{
			PRP = PRP + PPP[h][r];
		}
		RPR = 0;
		// ���� #1061
		for (int h = 0; h < num_harms; h++) 
		{
			RPR = RPR + PPP[h][r];
		}
		SS1 = (PPP[0][r] / PPR1[r] - 1) * 100.;
		SS2 = (RPR / PPR1[r]) * 100.;
		SS0 = PPP[0][r] - PPR1[r];

		// ������ ������ �� ���������� PPP, PPP1, PPP2 .... PPP8
		// � ��������������� �����. (���������� ���������!)
	}


	WD0 = 0;
	// ���� #1052
	for (int h = 0; h < num_harms; h++) 
	{
		WD[0][h] = 0;
		for (int r = 0; r < num_recs; r++) 
		{
			WD0 = WD0 + PPP[h][r] * DT / 60000.;
			WD[0][h] = WD[0][h] + PPP[h][r] * DT / 60000.;
		}
	}

	WD1 = WD0 - WD[0][0];
	// ���� #1053
	for (int h = 0; h < num_harms; h++)
	{
		WD[1][h] = WD[0][h] / WD0 * 100.;
	}

	WD4 = 0;
	// ���� #1056
	for (int h = 13; h < num_harms; h++) 
	{
		WD4 = WD4 + WD[1][h];
	}

	WD10 = 0;
	// ���� #1057
	for (int r = 0; r < num_recs; r++) 
	{
		WD10 = WD10 + PPR1[r] * DT / 60000.;
	}

	// ���� #1054
	for (int r = 0; r < num_recs; r++) 
	{
		PD[0][r] = 0;
		PD[1][r] = 0;
		// ���� #1058
		for (int h = 0; h < num_harms; h++)
		{
			PD[0][r] = PD[0][r] + PPP[h][r];
		}
		PD[1][r] = PD[0][r] - PPP[0][r];
		PD[2][r] = PD[1][r] / PD[0][r] * 100.;
	}

	// ����� ��������������� �������� ����������� �������!
	// ������ ����������� � ���� "���������� �������" (���������� ���������!)

	debug_file
		<< " ******************************************* Losses on Line ******************************************* " << endl;
	debug_file
		<< "|| Overall: " << WD0
		<< " || Main harmonics: " << WD[0][0]
		<< " || Methodics: " << WD10
		<< " || On higher harmonics: " << WD1
		<< " || Other losses: " << WD4
		<< endl;


	insert_end_separator();

	auto stop = high_resolution_clock::now();
	auto duration = duration_cast<seconds>(stop - start);
	debug_file << "Took to execute: " << duration.count() << " seconds." << std::endl;
	std::cout << "Execution has just finished!" << std::endl;
	return 0;
}