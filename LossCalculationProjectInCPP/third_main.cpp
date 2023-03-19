// ������������� ����������� ���������
#include <filesystem>
#include <iostream>
#include <fstream>
#include <string>
#include <io.h>         
#include <tuple>
#include <cmath>
#include <chrono>
#include <codecvt>
#include <fcntl.h>   
#include <iomanip>
#include <complex>
#include <vector>
#include <nlohmann/json.hpp>
#include <Eigen/Dense>
#include <Eigen/LU>
#include <OpenXLSX.hpp>
#include <cwchar>
#include <locale>
#include <Windows.h>

using std::filesystem::directory_iterator;
using json = nlohmann::json;

using namespace std;
using namespace std::chrono;
namespace fs = std::filesystem;

using namespace OpenXLSX;
using namespace Eigen;


// ������ ������� ��� ���������� ������ �� �������
void insert_gap(wofstream& report_file)
{
	report_file << L" \n";
}

void insert_start_separator(wofstream& report_file)
{
	report_file << L" \n";
	wstring start_title = L"=========================================================== * ������ * =========================================================== \n";
	report_file << start_title;
	report_file << L" \n";
}

void insert_end_separator(wofstream& report_file)
{
	report_file << L" \n";
	wstring end_title = L"=========================================================== * ����� * =========================================================== \n";
	report_file << end_title;
	report_file << L" \n";
}


int create_report_foler(wstring& dirname)
{
	LPCWSTR report_folder = dirname.c_str();
	
	if (fs::is_directory(report_folder)) fs::remove_all(report_folder);

	if (fs::create_directory(report_folder)) return 1;
	else return 0;
	
}

inline std::wstring convert(const std::string& as)
{
	if (as.empty())    return std::wstring();
	size_t reqLength = ::MultiByteToWideChar(CP_UTF8, 0, as.c_str(), (int)as.length(), 0, 0);
	std::wstring ret(reqLength, L'\0');
	::MultiByteToWideChar(CP_UTF8, 0, as.c_str(), (int)as.length(), &ret[0], (int)ret.length());

	return ret;
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
	tuple<int, int> get_range(int which, VectorXi titles_indexes, int num_pris)
	{
		// which = [1-6] ���� 6 ������������� � �������.
		tuple <int, int> pris_range;
		int r_start = titles_indexes(which - 1) + 1;
		int r_end = titles_indexes(which) - 1;
		
		if (which == num_pris) r_end++;
		pris_range = make_tuple(r_start, r_end);
		
		return pris_range;
	}
};

int sheets_counter = 1;
int titles_counter = 0;
int rows_counter = 0;
int phase_number;

int debug_breaker = 10;

// ������� ������. ��������� ������
double UM[8][50][700] = {0};  double AIM[8][50][700] = {0};
double FUM[8][50][700] = {0}; double FIM[8][50][700] = {0};

double UM1[8][50][700] = {0}; double UM2[8][50][700] = {0};
double AIM1[8][50][700] = {0}; double AIM2[8][50][700] = {0};

double PPR1[700] = {0}; double PPR2[700] = {0};

double PD[8][700] = {0}; double PPP[50][1000] = {0};
double PPP1[50][1000] = {0}; double PPP5[50][1000] = {0};
double PPP2[50][1000] = {0}; double PPP6[50][1000] = {0};
double PPP3[50][1000] = {0}; double PPP7[50][1000] = {0};
double PPP4[50][1000] = {0}; double PPP8[50][1000] = {0};

double WD[2][50] = {0}; double II[10] = {0};

enum main_harm_enum { knsu_e = 0, knsi_e, rmsu_e, rmsi_e, funu_e, funi_e, fu_e, fi_e };
double main_harm[8][8][700] = {0};

std::complex<double> UK(0, 0); std::complex<double> AIK(0, 0);
std::complex<double> AL(-0.5, 0.866025);

double SKU0 = 0., SKU2 = 0., SKI0 = 0., SKI2 = 0.;

const double PI = 3.14159265358979;

double FF = 0., SS2 = 0., SS0 = 0., PP1 = 0., PP2 = 0., RPR = 0., PRP = 0., WD0 = 0., WD1 = 0., WD4 = 0., WD10 = 0.;

std::complex<double> SS(0., 0.), SS1(0., 0.);

// ����� ���������� ��� ��������� �������! [MM, M, M1, MT, M10, M20, PR, K1, K2, K3, N1, N2, N3, MPR, MTR, MMT]
// ������ � ������������ ����
int M = 0, M1 = 0,  M10 = 0, M20 = 0, PR = 0, K1 = 0, MMT = 0;


const string current_dir_path = ".";

// ��������� ������� ���������!
void raschet(int& k, int& n, VectorXcd& UK1, VectorXcd& AIK1, VectorXd& XA, VectorXd& YA, VectorXd& OMP, VectorXd& GM, VectorXd& S, int& MPR, int& MTR, double& MM, int& MT, VectorXi& IH, wofstream& current_records, wofstream& voltage_records, wofstream& r11)
{
	// ��������� ������� ���� ��� �������� �������� � ������ �����. �� ���������� ��������� �� ������.
	// ��������� ���������� ����������� ��� ������������� �������� (���������) ���������� ������ ������� �������.
	const int M = MPR + MTR;
	const double MMT = MM / MT;
	const int M10 = 2 * M;
	const int M20 = 4 * M;

	int M1;
	if (M <= 6) M1 = M;
	if (M > 6) M1 = 6;

	//  ��� ������� (����) ����������� (���������) ����� ��������� ��������� ��������

	MatrixXcd HH11; HH11 = MatrixXcd::Zero(M, M); MatrixXcd HH12; HH12 = MatrixXcd::Zero(M, M);
	MatrixXcd HH21; HH21 = MatrixXcd::Zero(M, M); MatrixXcd HH22; HH22 = MatrixXcd::Zero(M, M);
	MatrixXcd HH13; HH13 = MatrixXcd::Zero(M, M); MatrixXcd HH31; HH31 = MatrixXcd::Zero(M, M);
	MatrixXcd HH23; HH23 = MatrixXcd::Zero(M, M); MatrixXcd HH41; HH41 = MatrixXcd::Zero(M, M);
	MatrixXcd HH32; HH32 = MatrixXcd::Zero(M, M); MatrixXcd HH14; HH14 = MatrixXcd::Zero(M, M);
	MatrixXcd HH42; HH42 = MatrixXcd::Zero(M, M); MatrixXcd HH24; HH24 = MatrixXcd::Zero(M, M);
	MatrixXcd HH34; HH34 = MatrixXcd::Zero(M, M); MatrixXcd HH33; HH33 = MatrixXcd::Zero(M, M);
	MatrixXcd HH44; HH44 = MatrixXcd::Zero(M, M); MatrixXcd HH43; HH43 = MatrixXcd::Zero(M, M);

	std::complex<double> DET10, SS1;
	VectorXcd DET1; DET1 = VectorXcd::Zero(M);
	
	MatrixXcd GG;  GG = MatrixXcd::Zero(M20, M20); MatrixXcd GG3; GG3 = MatrixXcd::Zero(M10, M20);
	MatrixXcd GG1; GG1 = MatrixXcd::Zero(M20, M20); MatrixXcd GG4; GG4 = MatrixXcd::Zero(M10, M10);
	MatrixXcd GG2; GG2 = MatrixXcd::Zero(M20, M20); MatrixXcd GG5; GG5 = MatrixXcd::Zero(M10, M10);

	MatrixXcd F; F = MatrixXcd::Zero(M, M); MatrixXcd F5; F5 = MatrixXcd::Zero(M, M);
	MatrixXcd F2; F2 = MatrixXcd::Zero(M, M); MatrixXcd F6; F6 = MatrixXcd::Zero(M, M);
	MatrixXcd F3; F3 = MatrixXcd::Zero(M, M); MatrixXcd F7; F7 = MatrixXcd::Zero(M, M);
	MatrixXcd F4; F4 = MatrixXcd::Zero(M, M); MatrixXd F10; F10 = MatrixXd::Zero(M, M);

	VectorXcd B; B = VectorXcd::Zero(M); VectorXcd B6; B6 = VectorXcd::Zero(M10);
	VectorXcd B1; B1 = VectorXcd::Zero(M20); VectorXcd B7; B7 = VectorXcd::Zero(M10);
	VectorXcd B4; B4 = VectorXcd::Zero(M20); VectorXcd B10; B10 = VectorXcd::Zero(M10);


	MatrixXcd LU;  LU = MatrixXcd::Zero(M, M); MatrixXcd LI;  LI = MatrixXcd::Zero(M, M);
	MatrixXcd LU1; LU1 = MatrixXcd::Zero(M, M); MatrixXcd LI1; LI1 = MatrixXcd::Zero(M, M);
	MatrixXcd LU2; LU2 = MatrixXcd::Zero(M, M); MatrixXcd LI2; LI2 = MatrixXcd::Zero(M, M);
	MatrixXcd LU3; LU3 = MatrixXcd::Zero(M, M); MatrixXcd LI3; LI3 = MatrixXcd::Zero(M, M);

	MatrixXcd Z; Z = MatrixXcd::Zero(M, M); MatrixXcd Y; Y = MatrixXcd::Zero(M, M);
	
	VectorXcd AA; AA = VectorXcd::Zero(M); 	MatrixXcd CC; CC = MatrixXcd::Zero(M, M);
	VectorXcd BB; BB = VectorXcd::Zero(M);	MatrixXcd DD; DD = MatrixXcd::Zero(M, M);

	MatrixXd HC1; HC1 = MatrixXd::Zero(M, M);
	MatrixXd HC3; HC3 = MatrixXd::Zero(M, M);

	VectorXcd EVI; EVI = VectorXcd::Zero(M);
	VectorXcd EVU; EVU = VectorXcd::Zero(M);

	MatrixXcd AU; AU = MatrixXcd::Zero(M, M);
	MatrixXcd AAI; AAI = MatrixXcd::Zero(M, M);

	VectorXd R0; R0 = VectorXd::Zero(M); VectorXd HI; HI = VectorXd::Zero(M);
	VectorXd R; R = VectorXd::Zero(M); VectorXd UXM; UXM = VectorXd::Zero(M);
	VectorXd R11; R11 = VectorXd::Zero(M); VectorXd AIXM; AIXM = VectorXd::Zero(M);

	VectorXcd UX; UX = VectorXcd::Zero(M); VectorXcd SM; SM = VectorXcd::Zero(M);
	VectorXcd AIX; AIX = VectorXcd::Zero(M); VectorXcd DET3; DET3 = VectorXcd::Zero(M);

	VectorXcd B5; B5 = VectorXcd::Zero(M20);

	MatrixXd HC2; HC2 = MatrixXd::Zero(M, M); MatrixXd G; G = MatrixXd::Zero(M, M);
	MatrixXd HC4; HC4 = MatrixXd::Zero(M, M); MatrixXd D; D = MatrixXd::Zero(M, M);
	MatrixXd XL; XL = MatrixXd::Zero(M, M); MatrixXd HC; HC = MatrixXd::Zero(M, M);
	MatrixXd XL1; XL1 = MatrixXd::Zero(M, M);

	MatrixXcd E; E = MatrixXcd::Zero(M, M);
	MatrixXcd F1; F1 = MatrixXcd::Zero(M, M);

	MatrixXd HH; HH = MatrixXd::Zero(M10, M10);


	// ������� �������� ������������ �������� ������� � ������� ���������� Eigen 
	MatrixXcd A1; A1 = MatrixXcd::Zero(M1, M1);
	MatrixXcd A2; A2 = MatrixXcd::Zero(M1, M1);
	VectorXd IPVT1; IPVT1 = VectorXd::Zero(M1);

	// ������� �������� � �������� AG. 3-x ������ [M1][M][M]. �1 - ���������!!!
	// �������� ������ � �������� �1, �������� � ���� ��� ���������� � �������� ���
	std::vector<MatrixXcd> AG;
	for (int i = 0; i < M1; i++)
	{
		MatrixXcd ag; ag = MatrixXcd::Zero(M, M);
		AG.push_back(ag);
	}

	if (PR == 1) PP1 = 0;
	if (PR == 2) PP2 = 0;

	double W = (double)k + 1.;
	std::complex<double> EX1(2.71828, 0.);

	// ������ � ���� #5 "��������� ����� ������" (���������� ���������!)
	
	// ���� #845
	for (int i = 0; i < M; i++)
	{
		R(i) = sqrt(S(i) / PI) / 1000.;
		HI(i) = R(i) / (2.) * sqrt(2. * PI * W * 50. * 4. * PI * OMP(i) * GM(i) / 20.);
		R0(i) = 1000. / (GM(i) * S(i));
		if (HI(i) < 1) R11(i) = R0(i) * (1. + pow(HI(i), (4./3.))); // ���������!
		if (HI(i) > 1) R11(i) = R0(i) * (HI(i) + 0.25 + 3./(64. * HI(i)));
		if (i == M-1)
		{
			for (auto record : R11)
			{
				r11 << record << L",";
			}; r11 << endl;
		}
	}
	
	// ���� #12
	HH = MatrixXd::Zero(M10, M10);

	// ���� #161
	for (int i = 0; i < M; i++)
	{
		for (int j = 0; j < M; j++)
		{
			if (i == j) D(i, i) = R(i);
			if (i != j) D(i, j) = sqrt(pow((XA(i) - XA(j)), 2) + pow((YA(i) - YA(j)), 2));
			HC(i, j) = sqrt(pow((XA(i) - XA(j)), 2) + pow((YA(i) + YA(j)), 2));
			E(i, j) = std::complex<double>(0.0, 0.0);
			E(i, i) = std::complex<double>(1.0, 0.0);
		}
	}

	// ���� #740
	for (int i = 0; i < M; i++)
	{
		for (int j = 0; j < M; j++)
		{
			XL1(i, j) = (0.145 * log10(1000. / D(i, j))) / (100. * PI);
		}
	}

	// ���� #743
	for (int i = 0; i < M; i++)
	{
		for (int j = 0; j < M; j++)
		{
			HC1(i, j) = 41.4 * pow(10., 6.) * log10(HC(i, j) / D(i, j));
		}
	}

	HC3 = HC1.inverse();
	F10 = HC1 * HC3;
	
	// ���� #744
	for (int i = 0; i < M; i++)
	{
		for (int j = 0; j < M; j++)
		{
			HC2(i, j) = HC3(i, j) * 2. * PI * 50.;
		}
	}

	// ���� #847
	for (int i = 0; i < M; i++)
	{
		for (int j = 0; j < M; j++)
		{
			XL(i, j) = XL1(i, j) * W * 2. * 50. * PI;
			HC4(i, j) = HC2(i, j) * W;
			double R10 = 0.0;
			if (i == j) Z(i, j) = std::complex<double>(R11(i), XL(i, j));
			if (i != j) Z(i, j) = std::complex<double>(R10, XL(i, j));
			if (i == j) G(i, j) = 0.00000004 * YA(i) / YA(i);
			if (i != j) G(i, j) = -0.00000004 * YA(0) / D(i, j);
			G(i, j) = 0.;
			Y(i, j) = std::complex<double>(G(i, j), HC4(i, j));
		}
	}

	// ���� #1300. �� ����� ������ ��������� �������!
	for (int iii = 0; iii < MT; iii++)
	{
		if (M != 3) goto label_767;
		// ���� #761
		for (int i = 0; i < MPR; i++)
		{
			B5(i) = UK1(i);
			B5(i + 3) = AIK1(i);
			B5(i + 6) = std::complex<double>(0., 0.);
			B5(i + 9) = std::complex<double>(0., 0.);
		}
	label_767:
		if (M != 4) goto label_768;
		// ���� #762
		for (int i = 0; i < MPR; i++)
		{
			B5(i) = UK1(i);
			B5(M - 1) = std::complex<double>(0., 0.);
			B5(i + M) = AIK1(i);
			B5(2 * M - 1) = std::complex<double>(0., 0.);
			B5(i + 2 * M) = std::complex<double>(0., 0.);
			B5(3 * M - 1) = std::complex<double>(0., 0.);
			B5(i + 3 * M) = std::complex<double>(0., 0.);
			B5(4 * M - 1) = std::complex<double>(0., 0.);
		}
	label_768:
		if (M != 6) goto label_769;
		// ���� #764
		for (int i = 0; i < MPR; i++)
		{
			B5(i) = UK1(i);
			B5(i + 3) = UK1(i);
			B5(i + M) = AIK1(i);
			B5(i + M + 3) = AIK1(i);
			B5(i + 2 * M) = std::complex<double>(0., 0.);
			B5(i + 2 * M + 3) = std::complex<double>(0., 0.);
			B5(i + 3 * M) = std::complex<double>(0., 0.);
			B5(i + 3 * M + 3) = std::complex<double>(0., 0.);
		}
	label_769:
		if (M != 7) goto label_770;
		// ���� #765
		for (int i = 0; i < MPR; i++)
		{
			B5(i) = UK1(i);
			B5(i + 3) = UK1(i);
			B5(M - 1) = std::complex<double>(0., 0.);
			B5(i + M) = AIK1(i);
			B5(i + M + 3) = AIK1(i);
			B5(2 * M - 1) = std::complex<double>(0., 0.);
			B5(i + 2 * M) = std::complex<double>(0., 0.);
			B5(i + 2 * M + 3) = std::complex<double>(0., 0.);
			B5(3 * M - 1) = std::complex<double>(0., 0.);
			B5(i + 3 * M) = std::complex<double>(0., 0.);
			B5(i + 3 * M + 3) = std::complex<double>(0., 0.);
			B5(4 * M - 1) = std::complex<double>(0., 0.);
		}

	label_770:
		if (M != 8) goto label_771;
		// ���� #766
		for (int i = 0; i < MPR; i++)
		{
			B5(i) = UK1(i);
			B5(i + 3) = UK1(i);
			B5(MPR + 1 - 1) = std::complex<double>(0., 0.);
			B5(MPR + 2 - 1) = std::complex<double>(0., 0.);
			B5(i + M) = AIK1(i);
			B5(i + M + 3) = AIK1(i);
			B5(2 * M - 1 - 1) = std::complex<double>(0., 0.);
			B5(2 * M - 1) = std::complex<double>(0., 0.);
			B5(i + 2 * M) = std::complex<double>(0., 0.);
			B5(i + 2 * M + 3) = std::complex<double>(0., 0.);
			B5(3 * M - 1 - 1) = std::complex<double>(0., 0.);
			B5(3 * M - 1) = std::complex<double>(0., 0.);
			B5(i + 3 * M) = std::complex<double>(0., 0.);
			B5(i + 3 * M + 3) = std::complex<double>(0., 0.);
			B5(4 * M - 1 - 1) = std::complex<double>(0., 0.);
			B5(4 * M - 1) = std::complex<double>(0., 0.);
		}

	label_771:
		
		// ���������� ������� LU
		// ������������ ������ ����������
		// �����! AU ���� ������������ !!!

		AU = Z * Y;
		SS1 = sqrt(AU(0, 0));
		
		// ���������� ����������� �������� ������� �U
		// �����! EVU ���� ������������ !!!
		EVU = AU.eigenvalues();
		EVU.reverseInPlace();

		
		// ������������ ������� �����������
		// ���� #20
		for (int j = 0; j < M; j++)
		{
			for (int i = 0; i < M; i++)
			{
				F(i, j) = pow(EVU(i), j);
				F1(i, j) = F(i, j);
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

		DET10 = A1.partialPivLu().determinant();
		// ���������� ������������ �����������

		// ���������� ����������� ������ �����������
		// ���� #21
		for (int j = 0; j < M1; j++)
		{   // ���� #211
			for (int ii = 0; ii < M1; ii++)
			{	// ���� #211
				for (int jj = 0; jj < M1; jj++)
				{
					F(ii, jj) = F1(ii, jj);
				}
			}
			// ���� #22
			for (int i = 0; i < M1; i++) {
				F(i, j) = pow(EVU(i), 0.5);
			}
			// ���� #202
			for (int ii = 0; ii < M1; ii++)
			{	// ���� #202
				for (int jj = 0; jj < M1; jj++)
				{
					A1(ii, jj) = F(ii, jj);
				}
			}

			// ������������ ����������� ������ �����������
			DET1(j) = A1.partialPivLu().determinant();
			// ���������� ����������� ������������� �����������		
		}

		F2 = AU * AU;
		F3 = F2 * AU;
		F4 = F3 * AU;
		F5 = F4 * AU;
		F6 = F5 * AU;
		F7 = F6 * AU;
		
		// ���� #441
		for (int i = 0; i < M1; i++)
		{
			for (int ii = 0; ii < M; ii++)
			{
				for (int jj = 0; jj < M; jj++)
				{
					if (i == 0) AG[0](ii, jj) = E(ii, jj);
					if (i == 1) AG[1](ii, jj) = AU(ii, jj);
					if (i == 2) AG[2](ii, jj) = F2(ii, jj);
					if (i == 3) AG[3](ii, jj) = F3(ii, jj);
					if (i == 4) AG[4](ii, jj) = F4(ii, jj);
					if (i == 5) AG[5](ii, jj) = F5(ii, jj);
					if (i == 6) AG[6](ii, jj) = F6(ii, jj);
					if (i == 7) AG[7](ii, jj) = F7(ii, jj);
				}
			}
		}

		// ���� #442
		LU = MatrixXcd::Zero(M, M);

		// ���� #410
		for (int i = 0; i < M1; i++)
		{
			DET3(i) = DET1(i) / DET10;
			for (int ii = 0; ii < M; ii++)
			{
				for (int jj = 0; jj < M; jj++)
				{
					LU(ii, jj) = LU(ii, jj) + AG[i](ii, jj) * DET3(i);
				}
			}
		}

		F3 = LU * LU;
		// ���������� ������� LI
		// ������������ ������ ����������
		AAI = Y * Z;
		// ���������� ����������� �������� ������� AAI
		EVI = AAI.eigenvalues();
		EVI.reverseInPlace();

		// ������������ ������� �����������
		// ���� #1120
		for (int j = 0; j < M; j++)
		{
			for (int i = 0; i < M; i++)
			{
				F(i, j) = pow(EVI(i),j);
				F1(i, j) = F(i, j);
			}
		}

		// ������������ ������� �����������
		// ���� #1201
		for (int i = 0; i < M1; i++)
		{
			for (int j = 0; j < M1; j++)
			{
				A1(i, j) = F(i, j);
			}
		}

		DET10 = A1.partialPivLu().determinant();
		// ���������� ������������ �����������
		

		// ���������� ����������� ������ �����������
		// ���� #121
		for (int j = 0; j < M1; j++)
		{
			// ���� #1211
			for (int ii = 0; ii < M1; ii++)
			{
				for (int jj = 0; jj < M1; jj++)
				{
					F(ii, jj) = F1(ii, jj);
				}
			}
			// ���� #122
			for (int i = 0; i < M1; i++)
			{
				F(i, j) = pow(EVI(i),0.5);
			}
			// ���� #1202
			for (int ii = 0; ii < M1; ii++)
			{
				for (int jj = 0; jj < M1; jj++)
				{
					A1(ii, jj) = F(ii, jj);
				}
			}
		
			// ������������ ����������� ������ �����������
			DET1(j) = A1.partialPivLu().determinant();
			// ���������� ����������� ������������� �����������
			
		
		}
		
		// ���������� �������� ������� �� �������
		F2 = AAI * AAI;
		F3 = F2 * AAI;
		F4 = F3 * AAI;
		F5 = F4 * AAI;
		F6 = F5 * AAI;
		F7 = F6 * AAI;

		// ���� #1441
		for (int i = 0; i < M1; i++)
		{
			for (int ii = 0; ii < M; ii++)
			{
				for (int jj = 0; jj < M; jj++)
				{
					if (i == 0) AG[0](ii, jj) = E(ii, jj);
					if (i == 1) AG[1](ii, jj) = AAI(ii, jj);
					if (i == 2) AG[2](ii, jj) = F2(ii, jj);
					if (i == 3) AG[3](ii, jj) = F3(ii, jj);
					if (i == 4) AG[4](ii, jj) = F4(ii, jj);
					if (i == 5) AG[5](ii, jj) = F5(ii, jj);
					if (i == 6) AG[6](ii, jj) = F6(ii, jj);
					if (i == 7) AG[7](ii, jj) = F7(ii, jj);
				}
			}
		}
		// ���� #1442
		LI = MatrixXcd::Zero(M, M);

		// ���� #1410
		for (int i = 0; i < M1; i++)
		{
			for (int ii = 0; ii < M; ii++)
			{
				for (int jj = 0; jj < M; jj++)
				{
					LI(ii, jj) = LI(ii, jj) + (DET1(i) * AG[i](ii, jj)) / (DET10);
				}
			}
		}

		F3 = LI * LI;
		double LM = MMT;

		// ������������ LU LU1 LU2 LU3 

		for (int n = 1; n < 3; n++)
		{
			double SA;
			if (n == 1) SA = -1.;
			if (n == 2) SA = 1.;

			// ���������� ��������� ���������������� �������
			for (int ii = 0; ii < M; ii++) {
				for (int jj = 0; jj < M; jj++) {
					LU1(ii, jj) = SA * MMT * LU(ii, jj);
				}
			}

			// ���������� ����������� �������� ������� LU1
			EVU = LU1.eigenvalues();
			EVU.reverseInPlace();

			// ������������ ������� �����������
			for (int i = 0; i < M; i++) {
				for (int j = 0; j < M; j++) {
					F(i, j) = pow(EVU(i), j);
				}
			}

			for (int ii = 0; ii < M; ii++) {
				for (int jj = 0; jj < M; jj++) {
					F1(ii, jj) = F(ii, jj);
				}
			}

			for (int ii = 0; ii < M1; ii++) {
				for (int jj = 0; jj < M1; jj++) {
					A1(ii, jj) = F(ii, jj);
				}
			}

			// ������������ ������� �����������
			DET10 = A1.partialPivLu().determinant();
			// ���������� ������������ �����������
			

			// ���������� ����������� ������ �����������
			for (int j = 0; j < M; j++) {
				for (int ii = 0; ii < M; ii++) {
					for (int jj = 0; jj < M; jj++) {
						F(ii, jj) = F1(ii, jj);
					}
				}

				for (int i = 0; i < M; i++) {
					F(i, j) = pow(EX1, EVU(i));
				}

				for (int ii = 0; ii < M1; ii++) {
					for (int jj = 0; jj < M1; jj++) {
						A1(ii, jj) = F(ii, jj);
					}
				}

				// ������������ ����������� ������ �����������
				DET1(j) = A1.partialPivLu().determinant();
				// ���������� ����������� ������������� �����������
				
			}

			F2 = LU1 * LU1;
			F3 = F2 * LU1;
			F4 = F3 * LU1;
			F5 = F4 * LU1;
			F6 = F5 * LU1;
			F7 = F6 * LU1;

			for (int i = 0; i < M1; i++) {
				for (int ii = 0; ii < M; ii++) {
					for (int jj = 0; jj < M; jj++) {
						if (i == 0) AG[0](ii, jj) = E(ii, jj);
						if (i == 1) AG[1](ii, jj) = LU1(ii, jj);
						if (i == 2) AG[2](ii, jj) = F2(ii, jj);
						if (i == 3) AG[3](ii, jj) = F3(ii, jj);
						if (i == 4) AG[4](ii, jj) = F4(ii, jj);
						if (i == 5) AG[5](ii, jj) = F5(ii, jj);
						if (i == 6) AG[6](ii, jj) = F6(ii, jj);
						if (i == 7) AG[7](ii, jj) = F7(ii, jj);
					}
				}
			}

			LU2 = MatrixXcd::Zero(M, M);

			for (int i = 0; i < M1; i++) {
				for (int ii = 0; ii < M; ii++) {
					for (int jj = 0; jj < M; jj++) {
						LU2(ii, jj) = LU2(ii, jj) + (DET1(i) * AG[i](ii, jj))/(DET10);
					}
				}
			}

			if (n == 2)  goto label_72411;

			for (int ii = 0; ii < M; ii++) {
				for (int jj = 0; jj < M; jj++) {
					LU3(ii, jj) = LU2(ii, jj);
				}
			}

		label_72411:
			F2 = LU3 * LU2;

		}


		// ������������ LI LI1 LI2 LI3 EVI

		for (int n = 1; n < 3; n++)
		{
			double SA;
			if (n == 1) SA = -1.;
			if (n == 2) SA = 1.;

			// ���������� ��������� ���������������� �������
			for (int ii = 0; ii < M; ii++) {
				for (int jj = 0; jj < M; jj++) {
					LI1(ii, jj) = SA * MMT * LI(ii, jj);
				}
			}

			// ���������� ����������� �������� ������� LI1
			EVI = LI1.eigenvalues();
			EVI.reverseInPlace();

			// ������������ ������� �����������
			for (int i = 0; i < M; i++) {
				for (int j = 0; j < M; j++) {
					F(i, j) = pow(EVI(i), j);
				}
			}

			for (int ii = 0; ii < M; ii++) {
				for (int jj = 0; jj < M; jj++) {
					F1(ii, jj) = F(ii, jj);
				}
			}

			for (int ii = 0; ii < M1; ii++) {
				for (int jj = 0; jj < M1; jj++) {
					A1(ii, jj) = F(ii, jj);
				}
			}

			// ������������ ������� �����������
			DET10 = A1.partialPivLu().determinant();
			// ���������� ������������ �����������
			

			// ���������� ����������� ������ �����������
			for (int j = 0; j < M; j++) {
				for (int ii = 0; ii < M; ii++) {
					for (int jj = 0; jj < M; jj++) {
						F(ii, jj) = F1(ii, jj);
					}
				}

				for (int i = 0; i < M; i++) {
					F(i, j) = pow(EX1, EVI(i));
				}

				for (int ii = 0; ii < M1; ii++) {
					for (int jj = 0; jj < M1; jj++) {
						A1(ii, jj) = F(ii, jj);
					}
				}

				// ������������ ����������� ������ �����������
				DET1(j) = A1.partialPivLu().determinant();
				// ���������� ����������� ������������� �����������
				
			}

			F2 = LI1 * LI1;
			F3 = F2 * LI1;
			F4 = F3 * LI1;
			F5 = F4 * LI1;
			F6 = F5 * LI1;
			F7 = F6 * LI1;

			for (int i = 0; i < M1; i++) {
				for (int ii = 0; ii < M; ii++) {
					for (int jj = 0; jj < M; jj++) {
						if (i == 0) AG[0](ii, jj) = E(ii, jj);
						if (i == 1) AG[1](ii, jj) = LI1(ii, jj);
						if (i == 2) AG[2](ii, jj) = F2(ii, jj);
						if (i == 3) AG[3](ii, jj) = F3(ii, jj);
						if (i == 4) AG[4](ii, jj) = F4(ii, jj);
						if (i == 5) AG[5](ii, jj) = F5(ii, jj);
						if (i == 6) AG[6](ii, jj) = F6(ii, jj);
						if (i == 7) AG[7](ii, jj) = F7(ii, jj);
					}
				}
			}

			LI2 = MatrixXcd::Zero(M, M);

			for (int i = 0; i < M1; i++) {
				for (int ii = 0; ii < M; ii++) {
					for (int jj = 0; jj < M; jj++) {
						LI2(ii, jj) = LI2(ii, jj) + (DET1(i) * AG[i](ii, jj))/(DET10);
					}
				}
			}

			if (n == 2)  goto label_82411;

			for (int ii = 0; ii < M; ii++) {
				for (int jj = 0; jj < M; jj++) {
					LI3(ii, jj) = LI2(ii, jj);
				}
			}

		label_82411:
			F2 = LI3 * LI2;

		}


		// ��������� ������ ��������!!!

		GG = MatrixXcd::Zero(M20, M20);
		GG1 = MatrixXcd::Zero(M20, M20);
		GG2 = MatrixXcd::Zero(M20, M20);

		for (int i = 0; i < M; i++) {
			GG(i, i) = 1.;
			GG(i, i + M) = 1.;
			GG(i + M, i + 2 * M) = 1.;
			GG(i + M, i + 3 * M) = 1.;
			GG1(i, i) = 1.;
			GG1(i, i + M) = 1.;
			GG1(i + M, i + 2 * M) = 1.;
			GG1(i + M, i + 3 * M) = 1.;
		}

		for (int i = 0; i < M; i++) {
			for (int j = 0; j < M; j++) {
				GG(i + 2 * M, j) = -LU(i, j);
				GG(i + 2 * M, j + M) = LU(i, j);
				GG(i + 3 * M, j + 2 * M) = -1. * LI(i, j);
				GG(i + 3 * M, j + 3 * M) = +LI(i, j);

				GG1(i + 2 * M, j) = LU3(i, j);
				GG1(i + 2 * M, j + M) = LU2(i, j);
				GG1(i + 3 * M, j + 2 * M) = LI3(i, j);
				GG1(i + 3 * M, j + 3 * M) = LI2(i, j);
			}
		}

		GG2 = GG1.inverse();

		for (int i = 0; i < M; i++) {
			for (int j = 0; j < M; j++) {
				HH11(i, j) = GG2(i, j);
				HH12(i, j) = GG2(i, j + M);
				HH13(i, j) = GG2(i, j + 2 * M);
				HH14(i, j) = GG2(i, j + 3 * M);
				HH21(i, j) = GG2(i + M, j);
				HH22(i, j) = GG2(i + M, j + M);
				HH23(i, j) = GG2(i + M, j + 2 * M);
				HH24(i, j) = GG2(i + M, j + 3 * M);
				HH31(i, j) = GG2(i + 2 * M, j);
				HH32(i, j) = GG2(i + 2 * M, j + M);
				HH33(i, j) = GG2(i + 2 * M, j + 2 * M);
				HH34(i, j) = GG2(i + 2 * M, j + 3 * M);
				HH41(i, j) = GG2(i + 3 * M, j);
				HH42(i, j) = GG2(i + 3 * M, j + M);
				HH43(i, j) = GG2(i + 3 * M, j + 2 * M);
				HH44(i, j) = GG2(i + 3 * M, j + 3 * M);
			}
		}

		F = LU3 * HH11;
		HH11 = LU * F;
		F = LU2 * HH21;
		HH21 = LU * F;
		F = LU3 * HH13;
		HH13 = LU * F;
		F = LU2 * HH23;
		HH23 = LU * F;
		F = LI3 * HH32;
		HH32 = LI * F;
		F = LI2 * HH42;
		HH42 = LI * F;
		F = LI3 * HH34;
		HH34 = LI * F;
		F = LI2 * HH44;
		HH44 = LI * F;

		GG3 = MatrixXcd::Zero(M10, M20);

		for (int i = 0; i < M; i++) {
			for (int j = 0; j < M; j++) {
				GG3(i, j) = -1. * HH11(i, j) + HH21(i, j);
				GG3(i, j + 2 * M) = -1. * HH13(i, j) + HH23(i, j);
				GG3(i, j + 3 * M) = -1. * Z(i, j);
				GG3(i + M, j + M) = -1. * HH32(i, j) + HH42(i, j);
				GG3(i + M, j + 2 * M) = -1. * Y(i, j);
				GG3(i + M, j + 3 * M) = -1. * HH34(i, j) + HH44(i, j);
			}
		}

		int K1 = 0;
		int K0 = 0;

		
		for (int j = 0; j < M20; j++) {
			
			if (IH(j) == 1) K1 = K1 + 1;
			if (IH(j) == 0) goto label_309;

			for (int i = 0; i < M10; i++) {
				GG4(i, K1-1) = -1. * GG3(i, j);
				B10(K1-1) = B5(j);
			}

			goto label_307;

		label_309:
			if (IH(j) == 0) K0 = K0 + 1;
				if (IH(j) == 1) goto label_307;
					for (int i = 0; i < M10; i++) {
						GG5(i, K0-1) = GG3(i, j);
					}
		label_307:
			continue;
		}

		B6 = GG4 * B10;
		B7 = GG5.inverse() * B6;

		K1 = 0;

		for (int j = 0; j < M20; j++) {
			if (IH(j) == 0) K1 = K1 + 1;
			if (IH(j) == 1) goto label_322;
			B5(j) = B7(K1-1);
		label_322:
			continue;
		}

		for (int i = 0; i < M; i++) {
			UK1(i) = B5(i);
			AIK1(i) = B5(i + M);
		}

		AA = Z * AIK1;
		BB = Y * UK1;
		CC = LI * LI3;
		DD = LI * LI2;

		for (int i = 0; i < M; i++) {
			B1(i) = UK1(i);
			B1(i + M) = AIK1(i);
			B1(i + 2 * M) = AA(i);
			B1(i + 3 * M) = BB(i);
		}

		B4 = GG.inverse() * B1;

		for (int i = 0; i < M; i++) {
			AA(i) = 0.;
			B(i) = 0.;
			B(i) = B4(i);
		}

		AA = LU3 * B;

		for (int i = 0; i < M; i++) {
			BB(i) = 0.;
			B(i) = 0.;
			B(i) = B4(i + M);
		}

		BB = LU2 * B;

		for (int i = 0; i < M; i++) {
			UX(i) = AA(i) + BB(i);
			if (LM == MMT) UK1(i) = UX(i);
			UXM(i) = sqrt(pow(real(UX(i)), 2.) + pow(imag(UX(i)), 2.));
		}

		for (int i = 0; i < M; i++) {
			AA(i) = 0.;
			B(i) = 0.;
			B(i) = B4(i + 2 * M);
		}

		AA = LI3 * B;

		for (int i = 0; i < M; i++) {
			BB(i) = 0.;
			B(i) = 0.;
			B(i) = B4(i + 3 * M);
		}

		BB = LI2 * B;

		for (int i = 0; i < M; i++) {

			AIX(i) = AA(i) + BB(i);
			if (LM == MMT) AIK1(i) = AIX(i);
			AIXM(i) = sqrt(pow(real(AIX(i)), 2.) + pow(imag(AIX(i)), 2.));

			if ((i == 0 and k == 0 and PR == 2) || (i == 0 and k > 0)) PPP1[k][n] = PPP1[k][n] + pow(AIXM(0), 2.) / 2. * R11(0);
			if ((i == 1 and k == 0 and PR == 2) || (i == 1 and k > 0)) PPP2[k][n] = PPP2[k][n] + pow(AIXM(1), 2.) / 2. * R11(1);
			if ((i == 2 and k == 0 and PR == 2) || (i == 2 and k > 0)) PPP3[k][n] = PPP3[k][n] + pow(AIXM(2), 2.) / 2. * R11(2);
			if ((i == 3 and k == 0 and PR == 2) || (i == 3 and k > 0)) PPP4[k][n] = PPP4[k][n] + pow(AIXM(3), 2.) / 2. * R11(3);
			if ((i == 4 and k == 0 and PR == 2) || (i == 4 and k > 0)) PPP5[k][n] = PPP5[k][n] + pow(AIXM(4), 2.) / 2. * R11(4);
			if ((i == 5 and k == 0 and PR == 2) || (i == 5 and k > 0)) PPP6[k][n] = PPP6[k][n] + pow(AIXM(5), 2.) / 2. * R11(5);
			if ((i == 6 and k == 0 and PR == 2) || (i == 6 and k > 0)) PPP7[k][n] = PPP7[k][n] + pow(AIXM(6), 2.) / 2. * R11(6);
			if ((i == 7 and k == 0 and PR == 2) || (i == 7 and k > 0)) PPP8[k][n] = PPP8[k][n] + pow(AIXM(7), 2.) / 2. * R11(7);
			
			if ((k == 0 and PR == 2) || (k > 0)) PPP[k][n] = PPP[k][n] + pow(AIXM(i), 2.) / 2. * R11(i);

			if (k == 0 and PR == 1)
				{PP1 = PP1 + pow(AIXM(i), 2.) / 2. * R11(i); }
			if (k == 0 and PR == 2)
				{PP2 = PP2 + pow(AIXM(i), 2.) / 2. * R11(i); }
			SM(i) = UX(i) * conj(AIX(i)) / 2.;		
		}
		for (auto record : AIXM)
		{
			current_records << record << L",";
		}; current_records << endl;
		for (auto record : UXM)
		{
			voltage_records << record << L",";
		}; voltage_records << endl;
	}
}


// ������� ������� ������� ���������!
int main() {

	auto start = high_resolution_clock::now();

	std::ifstream f(L"��������� ������.json");
	json data = json::parse(f);
	
	std::vector<double> tempX;
	std::vector<double> tempY;
	
	double tempPhaseChar[3] = {data["mat_prop_list"][0], data["mat_prop_list"][1], data["mat_prop_list"][2]};
	double tempTrossChar[3] = {data["mat_prop_list"][3], data["mat_prop_list"][4], data["mat_prop_list"][5]};

	for (int pass = 0; pass < data["spatial_config"].size()/2; pass++)
	{
		tempX.push_back(data["spatial_config"][pass*2]);
		tempY.push_back(data["spatial_config"][(pass * 2)+1]);
	}

	// ���������� �������� ���������� �������
	int num_phases;
	int num_tross;
	int num_harms;
	int num_recs; // ~ ���������� ��������� � ��������� ��� ������� �������������

	int num_lines = (int)data["line_type"]; // 1 - ����������, 2 - ���������� �����
	int num_pris = (int)data["number_of_prisoeds"]; // ����� ���������� ������������� � ������� ���������� 
	int pris_num = (int)data["which_prisoed"]; // 1 - ��� ������, 2 - ��� ������, 3 - �������, .. (��������� ������)

	if (num_lines == 1) { num_phases = 3; num_tross = 1; }
	else { num_phases = 6; num_tross = 1; }

	int all_wires = num_phases + num_tross;

	double MM = (double)data["line_length"];
	int MPR = num_phases;
	int MTR = num_tross;
	double DT = (double)data["record_frequency"];
	int MT = (int)data["groud_count"];

	VectorXi titles_indexes(num_pris + 1);

	VectorXcd UK1, AIK1;
	UK1 = VectorXcd::Zero(all_wires);
	AIK1 = VectorXcd::Zero(all_wires);

	// ������� ���������� ��� � ������ �����
	VectorXd XA(all_wires), YA(all_wires), OMP(all_wires), GM(all_wires), S(all_wires);
	
	for (int phase = 0; phase < num_phases; phase++) 
	{
		OMP(phase) = tempPhaseChar[0];
		GM(phase) = tempPhaseChar[1];
		S(phase) = tempPhaseChar[2];

		XA(phase) = tempX.at(phase);
		YA(phase) = tempY.at(phase);
	}
	OMP(num_phases) = tempTrossChar[0];
	GM(num_phases) = tempTrossChar[1];
	S(num_phases) = tempTrossChar[2];
	XA(num_phases) = tempX.at(num_phases);
	YA(num_phases) = tempY.at(num_phases);


	// ������ � ����������� ����� 
	VectorXi IH(all_wires*4);
	for (int i = 0; i < all_wires * 4; i++) { IH(i) = (int)data["tross_config"][i];}
	

	auto garbage = _setmode(_fileno(stdout), _O_U16TEXT);
	const locale utf8_locale = locale(locale(), new codecvt_utf8<wchar_t>());

	string prisoed_report_name = data["prisoed_name"];
	wstring c_prisoed_report_name = convert(prisoed_report_name);

	string excel_file_name = data["excel_filepath"];
	wstring c_excel_file_name = convert(excel_file_name);

	int is_successful = create_report_foler(c_prisoed_report_name);
	if (!is_successful) return 0;

	wstring working_directory = L".\\" + c_prisoed_report_name;

	wofstream report_file(working_directory + L"\\����� " + c_prisoed_report_name + L".txt"); report_file.imbue(utf8_locale);

	wofstream current_records(working_directory + L"\\����� �����.csv"); current_records.imbue(utf8_locale);
	wofstream voltage_records(working_directory + L"\\����� ����������.csv"); voltage_records.imbue(utf8_locale);
	wofstream r11(working_directory + L"\\R11.csv"); r11.imbue(utf8_locale);

	wofstream ppp(working_directory + L"\\PPP.csv"); ppp.imbue(utf8_locale); wofstream ppp4(working_directory + L"\\PPP4.csv"); ppp4.imbue(utf8_locale);
	wofstream ppp1(working_directory + L"\\PPP1.csv"); ppp1.imbue(utf8_locale); wofstream ppp5(working_directory + L"\\PPP5.csv"); ppp5.imbue(utf8_locale);
	wofstream ppp2(working_directory + L"\\PPP2.csv"); ppp2.imbue(utf8_locale); wofstream ppp6(working_directory + L"\\PPP6.csv"); ppp6.imbue(utf8_locale);
	wofstream ppp3(working_directory + L"\\PPP3.csv"); ppp3.imbue(utf8_locale); wofstream ppp7(working_directory + L"\\PPP7.csv"); ppp7.imbue(utf8_locale);

	if (num_lines == 1) 
	{
		current_records << L"���� �," << L"���� �," << L"���� �," << L"����," << endl;
		voltage_records << L"���� �," << L"���� �," << L"���� �," << L"����," << endl;
		r11 << L"���� �," << L"���� �," << L"���� �," << L"����," << endl;
	}
	else 
	{
		current_records << L"���� �1," << L"���� �1," << L"���� �1," << L"���� �2," << L"���� �2," << L"���� �2," << L"����," << endl;
		voltage_records << L"���� �1," << L"���� �1," << L"���� �1," << L"���� �2," << L"���� �2," << L"���� �2," << L"����," << endl;
		r11 << L"���� �1," << L"���� �1," << L"���� �1," << L"���� �2," << L"���� �2," << L"���� �2," << L"����," << endl;
	}


	insert_start_separator(report_file);
	

	XLDocument doc;

	doc.open(".\\" + (string)data["excel_filepath"]); // ��������� Excel ����
	auto workbook = doc.workbook();
	auto check_worksheet = workbook.worksheet(workbook.worksheetNames()[0]);

	int worksheets_count = doc.workbook().worksheetCount();
	int f_columns_count = check_worksheet.columnCount();
	int f_rows_count = check_worksheet.rowCount();

	// ����������, � ����������� �� ���������� �������� � ������� � ���������� ��� ������� �������������
	num_harms = f_columns_count / 2;

	// ����������� ����� � ������ ������ ��������� ��� ������� ������������� � �����
	titles_counter = 0;
	for (auto& row : check_worksheet.rows())
	{
		for (auto& cell_value : row.cells(1))
		{
			if (cell_value.value().typeAsString() == "string")
			{
				titles_indexes(titles_counter) = cell_value.cellReference().row();
				titles_counter++;
			}
		}
	}
	titles_indexes(num_pris) = f_rows_count;

	PhaseSheetsHandler phaser;
	CustomRangePairs ranger;
	ProsoedRowRange prisoeder;

	// ����������, � ����������� �� ���������� ����� � ������� ��� ������� �������������
	const auto [first, second] = prisoeder.get_range(pris_num, titles_indexes, num_pris);
	num_recs = (second - first) + 1;
	
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

	auto half_stop = high_resolution_clock::now();
	auto half_duration = duration_cast<seconds>(half_stop - start);
	wstring excel_read_title = L"Excel ���� ��� ������� ��������!";
	wstring excel_read_time_title = L"������ ����� ������ ";
	wstring second_title = L" ������.";
	report_file << excel_read_title << endl;
	report_file << excel_read_time_title << half_duration.count() << second_title << std::endl;
	
	insert_gap(report_file);
	
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
			UM[p][0][r] = main_harm[funu][p][r] * sqrt(2.);
			FUM[p][0][r] = main_harm[fu][p][r] * (PI / 180.);
			UM1[p][0][r] = UM[p][0][r] * cos(FUM[p][0][r]);
			UM2[p][0][r] = UM[p][0][r] * sin(FUM[p][0][r]);
		};
	};
	// ���� #14 � ��������
	for (int r = 0; r < num_recs; r++)
	{
		for (int p = 0; p < num_phases; p++)
		{
			for (int h = 1; h < num_harms + 1; h++)
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
			for (int h = 1; h < num_harms + 1; h++)
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
		for (int k = 0; k < num_harms + 1; k++) // 50 ������
		{
			//label_1500:
			PR = 0;
		label_1700:
			PR = PR + 1;

			for (int t = 0; t < num_phases; t++)
			{
				UK1(t) = std::complex<double>(UM1[t][k][n], UM2[t][k][n]);
				AIK1(t) = std::complex<double>(AIM1[t][k][n], AIM2[t][k][n]);
			}

			for (int t = 0; t < num_tross; t++)
			{
				UK1(num_phases + t) = std::complex<double>(0.0, 0.0);
				AIK1(num_phases + t) = std::complex<double>(0.0, 0.0);
			}

			if (k > 0) goto label_1111;
			if (k == 0 && PR == 2) goto label_1111;

			for (int l = 0; l < num_lines; l++)
			{
				UK = (UK1(l + l * (2)) + UK1(l + 1 + l * (2)) * AL + UK1(l + 2 + l * (2)) * std::pow(AL, 2.)) / (3.);

				UK1(l) = UK;
				UK1(l + 1 + l * (2)) = UK * std::pow(AL, 2.);
				UK1(l + 2 + l * (2)) = UK * AL;

				AIK = (AIK1(l + l * (2)) + AIK1(l + 1 + l * (2)) * AL + AIK1(l + 2 + l * (2)) * std::pow(AL, 2.)) / (3.);

				AIK1(l + l * (2)) = AIK;
				AIK1(l + 1 + l * (2)) = AIK * std::pow(AL, 2.);
				AIK1(l + 2 + l * (2)) = AIK * AL;
			}

		label_1111:			 

			// ����� ��������� �������!
			raschet(k, n, UK1, AIK1, XA, YA, OMP, GM, S, MPR, MTR, MM, MT, IH, current_records, voltage_records, r11);
			/*if (debug_breaker == 0) break;
			debug_breaker--;*/

			if (k == 0 && PR == 1) PPR1[n] = PP1;
			if (k == 0 && PR == 2) PPR2[n] = PP2;
			if (k == 0 && PR == 1) goto label_1700;
			if (PR == 2) continue; //goto label_1500;
		}
		//if (debug_breaker == 0) break;
	}


	// ���� #1501
	for (int r = 0; r < num_recs; r++)
	{
		PRP = 0;
		// ���� #1060
		for (int h = 0; h < num_harms + 1; h++)
		{
			PRP = PRP + PPP[h][r];
		}
		RPR = 0;
		// ���� #1061
		for (int h = 1; h < num_harms + 1; h++)
		{
			RPR = RPR + PPP[h][r];
		}
		SS1 = (PPP[0][r] / PPR1[r] - 1) * 100.;
		SS2 = (RPR / PPR1[r]) * 100.;
		SS0 = PPP[0][r] - PPR1[r];

		// ������ ������ �� ���������� PPP, PPP1, PPP2 .... PPP8
		// � ��������������� �����. (���������� ���������!)
		for (int h = 0; h < num_harms + 1; h++) 
		{
			if (num_lines == 1) 
			{
				ppp1 << PPP1[h][r] << L",";
				ppp2 << PPP2[h][r] << L",";
				ppp3 << PPP3[h][r] << L",";
				ppp4 << PPP4[h][r] << L",";
			}
			else 
			{
				ppp1 << PPP1[h][r] << L",";
				ppp2 << PPP2[h][r] << L",";
				ppp3 << PPP3[h][r] << L",";
				ppp4 << PPP4[h][r] << L",";
				ppp5 << PPP5[h][r] << L",";
				ppp6 << PPP6[h][r] << L",";
				ppp7 << PPP7[h][r] << L",";
			}
			ppp << PPP[h][r] << L",";
		} 
		
		ppp << PRP << L"," << PPR1[r] << L"," << SS0 << L"," << RPR << L"," << SS1 << L"," << SS2 << endl;

		if (num_lines == 1) 
		{
			ppp1 << endl;
			ppp2 << endl;
			ppp3 << endl;
			ppp4 << endl;
		}
		else
		{
			ppp1 << endl;
			ppp2 << endl;
			ppp3 << endl;
			ppp4 << endl;
			ppp5 << endl;
			ppp6 << endl;
			ppp7 << endl;
		}


	}


	WD0 = 0;
	// ���� #1052
	for (int h = 0; h < num_harms + 1; h++)
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
	for (int h = 0; h < num_harms + 1; h++)
	{
		WD[1][h] = WD[0][h] / WD0 * 100.;
	}

	WD4 = 0.;
	// ���� #1056
	for (int h = 13; h < num_harms + 1; h++)
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
		for (int h = 0; h < num_harms + 1; h++)
		{
			PD[0][r] = PD[0][r] + PPP[h][r];
		}
		PD[1][r] = PD[0][r] - PPP[0][r];
		PD[2][r] = PD[1][r] / PD[0][r] * 100.;
	}

	// ����� ��������������� �������� ����������� �������!
	// ������ ����������� � ���� "���������� �������" (���������� ���������!)
	
	wstring title = L"************************ ��������� ������ ������� � �������� � ��������� ������, ����������� � ���������� '" + c_excel_file_name.substr(0, c_excel_file_name.find_last_of(L".")) + L"' ************************";
	wstring subtitle = L"������ ��� ������������� �" + to_wstring(pris_num) + L": '" + c_prisoed_report_name + L"'";
	wstring overall = L"|| �����: ";
	wstring main_harmonics = L" || �������� ���������: ";
	wstring methodics = L" || ���������: ";
	wstring higher_harmonics = L" || ������ ���������: ";
	wstring other = L" || ������: ";


	report_file
		<< title << endl << endl <<
		subtitle << endl;
	
	insert_gap(report_file);

	report_file
		<< overall << WD0
		<< main_harmonics << WD[0][0]
		<< methodics << WD10
		<< higher_harmonics << WD1
		<< other << WD4;
	for (int i = 0; i < 13; i++)
	{
		wstring detail = L" || W" + to_wstring(i+1);
		report_file << detail << L"%: " << WD[1][i];
	}
		
	report_file << endl;

	insert_gap(report_file);

	wstring finish_time_title = L"����� ������ �������: ";
	wstring calculation_time_title = L"������ ����� �������: ";

	auto stop = high_resolution_clock::now();
	auto duration = duration_cast<seconds>(stop - start);
	report_file << calculation_time_title << duration.count() - half_duration.count() << second_title << std::endl;
	report_file << finish_time_title << duration.count() << second_title << std::endl;

	insert_end_separator(report_file);

	return 0;
}