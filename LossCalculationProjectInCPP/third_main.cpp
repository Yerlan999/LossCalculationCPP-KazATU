// Импортироване необходимых библиотек
#include <iostream>
#include <fstream>
#include <string>
#include <chrono>
#include <tuple>
#include <cmath>
#include <complex>
//#include <Eigen/Dense>
#include <OpenXLSX.hpp>

using namespace std;
using namespace std::chrono;

using namespace OpenXLSX;
//using namespace Eigen;

std::ofstream debug_file;

// Прочие функции для оформления вывода на консоль
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


// Функция для конвертации текста в цифру с плав. точкой
float stringToFloat(string s)
{
	float toFloat;
	stringstream converter(s);
	converter >> toFloat;
	return toFloat;
}

// Функция для конвертации текста в цифру
int stringToInt(string s)
{
	int toInt;
	stringstream converter(s);
	converter >> toInt;
	return toInt;
}


// Класс помощник для получения данных по Амплитуде и Фазе гармоник
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

// Класс помощник для получения данных по Фазам цепи
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


// Класс помощник для составления диапазона для определенного присоединения
class ProsoedRowRange 
{
public:
	tuple<int, int> get_range(int which, int titles_indexes[], int num_pris)
	{
		// which = [1-6] Если 6 присоединении в системе.
		tuple <int, int> pris_range;
		int r_start = titles_indexes[which - 1] + 1;
		int r_end = titles_indexes[which] - 1;
		
		if (which == num_pris) r_end++;
		pris_range = make_tuple(r_start, r_end);
		
		return pris_range;
	}
};

// Объявление основных переменных системы
const int num_pris = 6; // Общее количество присоединении в системе подстанции 
const int pris_num = 1; // 1 - для первой, 2 - для второй, 3 - третьей, .. (совершить расчет)
const int num_phases = 3;
const int num_tross = 1;
const int num_harms = 49;
const int num_recs = 560; // ~ Количество измерении в документе для каждого присоединения
int titles_indexes[(num_pris+1)];
int sheets_counter = 1;
int titles_counter = 0;
int rows_counter = 0;
int phase_number;

// Матрицы данных. Параметры режима
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


// Данные о зазмемлении линии
const int ground_config[16] = { 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0 };

std::complex<double> UK1[8] = {0}; std::complex<double> AIK1[8] = {0};

std::complex<double> UK10(0, 0); std::complex<double> AIK10(0, 0);
std::complex<double> UK11(0, 0); std::complex<double> AIK11(0, 0);
std::complex<double> UK12(0, 0); std::complex<double> AIK12(0, 0);
std::complex<double> AL(-0.5, 0.866025);

float SKU0 = 0, SKU2 = 0, SKI0 = 0, SKI2 = 0;

const double PI = 3.14159265358979f;

float RZ = 35.3;
float FF = 0, SS2 = 0, SS0 = 0, SS1 = 0, PP1 = 0, PP2 = 0, RPR = 0, PRP = 0, WD0 = 0, WD1 = 0, WD4 = 0, WD10 = 0;

// Общие переменной для расчетной функции! [MM, M, M1, MT, M10, M20, PR, K1, K2, K3, N1, N2, N3, MPR, MTR, MMT]
// Данные о конфигурации опор
int M = 0, M1 = 0,  M10 = 0, M20 = 0, PR = 0, K1 = 0, K2 = 0, K3 = 0, N1 = 0, N2 = 0, N3 = 0, MMT = 0;

const int MM = 5;
const int MPR = 3;
const int MTR = 1;
const float DT = 2.5;
const int MT = 5;

// Массивы параметров фаз и тросса линии 
const float XA[num_phases+num_tross] = {0.0, 6.3, 4.2, 2.1};
const float YA[num_phases + num_tross] = {19.0, 19.0, 25.0, 28.0};
const float OMP[num_phases + num_tross] = {1.0, 1.0, 1.0, 4000.0};
const float GM[num_phases + num_tross] = {35.336, 35.336, 35.336, 17.336};
const float S[num_phases + num_tross] = {150.0, 150.0, 150.0, 50.0};


// Расчетная функция программы!
void raschet()
{
	// declare the variables first...
}

// Главная функция запуска программы!
int main() {
	
	auto start = high_resolution_clock::now();

	debug_file.open("debug.txt", std::ios_base::app);

	XLDocument doc;
	doc.open("./Promzona.xlsx"); // Открываем Excel файл
	auto workbook = doc.workbook();
	auto check_worksheet = workbook.worksheet(workbook.worksheetNames()[0]);

	int worksheets_count = doc.workbook().worksheetCount();
	int f_columns_count = check_worksheet.columnCount();
	int f_rows_count = check_worksheet.rowCount();

	// Вывод основных данных о Excel файле
	insert_start_separator();
	debug_file << "Workbook's WorkSheets count: " << worksheets_count << endl;

	insert_gap();
	debug_file << "First Worksheet's Columns count: " << f_columns_count << endl;
	debug_file << "First Worksheet's Rows count: " << f_rows_count << endl;
	insert_gap();

	// Определение начал и концов данных измерении для каждого присоединения в файле
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

	// Чтение EXCEL файла и запись полученных данных
	for (auto& worksheet_name : workbook.worksheetNames())
	{
		// Для каждго листа в документе ... ["Sheet1", "Sheet2", "Sheet3", "Sheet4", "Sheet5", "Sheet6"]
		auto worksheet = workbook.worksheet(worksheet_name);
		int w_columns_count = worksheet.columnCount();
		int w_rows_count = worksheet.rowCount();

		if (!(sheets_counter % 2 == 0)) { phase_number = phaser.get_phase_number(sheets_counter); }
		else { phase_number = (sheets_counter - 2) / 2; };

		// В зависимости от рассчитываемого присоединения...
		for (auto& row : worksheet.rows(first, second)) // для каждой строки данного присоединения
		{

			std::deque<XLCellValue> cell(row.values()); // выбор всех ячеек в данной строке

			if (sheets_counter % 2 == 0)                        // Четные листы [Sheet2, Sheet4, Sheet6]
			{
				// Амплитуды и фазы для Тока
				for (int h = 1; h < ((w_columns_count - (w_columns_count - (num_harms * 2))) / 2) + 1; h++) // h = [1-49]
				{
					const auto [amp, pha] = ranger.get_range_pairs(h);
					try { AIM[phase_number][h][rows_counter] = cell.at(amp).get<double>(); }
					catch (XLValueTypeError) { AIM[phase_number][h][rows_counter] = (double)cell.at(amp).get<int>(); }
					try { FIM[phase_number][h][rows_counter] = cell.at(pha).get<double>(); }
					catch (XLValueTypeError) { FIM[phase_number][h][rows_counter] = (double)cell.at(pha).get<int>(); }
				}

				// Оставшиеся (последние) 8 столбцов. Данные Основной Гармоники
				for (int i = 0; i < w_columns_count - (num_harms * 2); i++)
				{
					try { main_harm[i][phase_number][rows_counter] = cell.at(num_harms * 2 + i).get<double>(); }
					catch (XLValueTypeError) { main_harm[i][phase_number][rows_counter] = (double)cell.at(num_harms * 2 + i).get<int>(); }
				}

				ranger.reset();
			}
			else                                                // Нечетные листы [Sheet1, Sheet3, Sheet5]
			{
				// Амплитуды и фазы для Напряжения 
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

	// Листы EXCEL файла прочитаны. Предварительные матрицы составлены.
	// Проведение расчетов!

	// Необходимые енумераторы для матрицы основной гармоники
	main_harm_enum knsu, knsi, rmsu, rmsi, funu, funi, fu, fi;
	knsu = knsu_e; rmsu = rmsu_e; funu = funu_e; fu = fu_e;
	knsi = knsi_e; rmsi = rmsi_e; funi = funi_e; fi = fi_e;


	// Цикл #13 в Фортране
	for (int r = 0; r < num_recs; r++)
	{
		for (int p = 0; p < num_phases; p++)
		{
			UM[p][0][r] = main_harm[funu][0][r] * sqrt(2);
			FUM[p][0][r] = main_harm[fu][0][r] * (PI / 180);
			UM1[p][0][r] = UM[p][0][r] * cos(FUM[p][0][r]);
			UM2[p][0][r] = UM[p][0][r] * sin(FUM[p][0][r]);
		};
	};
	// Цикл #14 в Фортране
	for (int r = 0; r < num_recs; r++)
	{
		for (int p = 0; p < num_phases; p++)
		{
			for (int h = 1; h < num_harms; h++)
			{
				UM[p][h][r] = UM[p][h][r] * UM[p][0][r] / 100;
				FUM[p][h][r] = FUM[p][h][r] * PI / 180;
				UM1[p][h][r] = UM[p][h][r] * cos(FUM[p][h][r]);
				UM2[p][h][r] = UM[p][h][r] * sin(FUM[p][h][r]);
			}
		}
	}
	// Цикл #16 в Фортране
	for (int r = 0; r < num_recs; r++)
	{
		for (int p = 0; p < num_phases; p++)
		{
			AIM[p][0][r] = main_harm[funi][p][r]*sqrt(2);
			FIM[p][0][r] = main_harm[fi][p][r]*PI/180;
			AIM1[p][0][r] = AIM[p][0][r]*cos(FIM[p][0][r]);
			AIM2[p][0][r] = AIM[p][0][r]*sin(FIM[p][0][r]);
		}
	}
	// Цикл #117 в Фортране
	for (int r = 0; r < num_recs; r++)
	{
		for (int p = 0; p < num_phases; p++)
		{
			for (int h = 1; h < num_harms; h++)
			{
				AIM[p][h][r] = AIM[p][h][r]*AIM[p][0][r]/100;
				FIM[p][h][r] = FIM[p][h][r]*PI/180;
				AIM1[p][h][r] = AIM[p][h][r]*cos(FIM[p][h][r]);
				AIM2[p][h][r] = AIM[p][h][r]*sin(FIM[p][h][r]);
			}
		}
	}
	
	M = MPR + MTR;
	MMT = MM / MT;
	
	if (M <= 6) M1 = M;
	if (M > 6) M1 = 6;

	M10 = 2 * M;
	M20 = 4 * M;

	// Цикл #1500. Главный!
	for (int n = 0; n < num_recs; n++) 
	{
		for (int k = 0; k < num_harms; k++)
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
			
			UK10 = (UK1[0] + UK1[1] + UK1[2]) / (double)3;
			UK11 = (UK1[0] + UK1[1] * AL + UK1[2] * std::pow(AL, 2)) / (double)3;
			UK12 = (UK1[0] + UK1[1] * std::pow(AL, 2) + UK1[2] * AL) / (double)3;
			
			// Следующие 2 строки не несут никакой практической пользы в программе, но есть в коде Фортрана! Можно удалить.
			SKU2 = sqrt(std::pow(real(UK12), 2) + std::pow(imag(UK12), 2)) / sqrt(std::pow(real(UK11), 2) + std::pow(imag(UK11), 2)) * 100;
			SKU0 = sqrt(std::pow(real(UK10), 2) + std::pow(imag(UK10), 2)) / sqrt(std::pow(real(UK11), 2) + std::pow(imag(UK11), 2)) * 100;
			
			UK1[0] = UK11;
			UK1[1] = UK11 * std::pow(AL, 2);
			UK1[2] = UK11 * AL;
			
			AIK10 = (AIK1[0] + AIK1[1] + AIK1[2]) / (double)3;
			AIK11 = (AIK1[0] + AIK1[1] * AL + AIK1[2] * std::pow(AL, 2)) / (double)3;
			AIK12 = (AIK1[0] + AIK1[1] * std::pow(AL, 2) + AIK1[2] * AL) / (double)3;
			
			// Следующие 2 строки не несут никакой практической пользы в программе, но есть в коде Фортрана! Можно удалить.
			SKI2 = sqrt(std::pow(real(AIK12), 2) + std::pow(imag(AIK12), 2)) / sqrt(std::pow(real(AIK11), 2) + std::pow(imag(AIK11), 2)) * 100;
			SKI0 = sqrt(std::pow(real(AIK10), 2) + std::pow(imag(AIK10), 2)) / sqrt(std::pow(real(AIK11), 2) + std::pow(imag(AIK11), 2)) * 100;
			
			AIK1[0] = AIK11;
			AIK1[1] = AIK11 * std::pow(AL, 2);
			AIK1[2] = AIK11 * AL;
		
		label_1111:
			
			// Вызов расчетной функции!
			raschet();
			
			if (k == 1 && PR == 1) PPR1[n] = PP1;
			if (k == 1 && PR == 2) PPR2[n] = PP2;
			if (k == 1 && PR == 1) goto label_1700;
			if (PR == 2) continue; //goto label_1500;
		}
	}


	// Цикл #1501.
	for (int r = 0; r < num_recs; r++) {
		PRP = 0;
		// Цикл #1060.
		for (int h = 0; h < num_harms; h++) {
			PRP = PRP + PPP[h][r];
		}
		RPR = 0;
		// Цикл #1061.
		for (int h = 0; h < num_harms; h++) {
			RPR = RPR + PPP[h][r];
		}
		SS1 = (PPP[0][r] / PPR1[r] - 1) * 100;
		SS2 = (RPR / PPR1[r]) * 100;
		SS0 = PPP[0][r] - PPR1[r];
		
		// Запись данных из переменных PPP, PPP1, PPP2 .... PPP8
		// в соответствующие файлы. (Пропущенно намеренно!)
	}


	WD0 = 0;
	// Цикл #1052.
	for (int h = 0; h < num_harms; h++) {
		WD[0][h] = 0;
		for (int r = 0; r < num_recs; r++) {
			WD0 = WD0 + PPP[h][r] * DT / 60000;
			WD[0][h] = WD[0][h] + PPP[h][r] * DT / 60000;
		}
	}

	WD1 = WD0 - WD[0][0];
	// Цикл #1053.
	for (int h = 0; h < num_harms; h++) {
		WD[1][h] = WD[0][h] / WD0 * 100;
	}

	WD4 = 0;
	// Цикл #1056.
	for (int h = 13; h < num_harms; h++) {
		WD4 = WD4 + WD[1][h];
	}

	WD10 = 0;
	// Цикл #1057.
	for (int r = 0; r < num_recs; r++) {
		WD10 = WD10 + PPR1[r] * DT / 60000;
	}

	// Цикл #1054.
	for (int r = 0; r < num_recs; r++) {
		PD[0][r] = 0;
		PD[1][r] = 0;
		// Цикл #1058.
		for (int h = 0; h < num_harms; h++) {
			PD[0][r] = PD[0][r] + PPP[h][r];
		}
		PD[1][r] = PD[0][r] - PPP[0][r];
		PD[2][r] = PD[1][r] / PD[0][r] * 100;
	}

	// Вывод предварительных значении результатов расчета!
	// Запись результатов в файл "Результаты расчета" (Пропущенно намеренно!)
	
	//                                            ПОТЕРИ:	
	
	//           Всего        Основная гармоника       Методика    На высших гармониках  Прочие потери 
	debug_file << WD0 << " || " << WD[0][0] << " || " << WD10 << " || " << WD1 << " || " << WD4 << endl;


	insert_end_separator();

	auto stop = high_resolution_clock::now();
	auto duration = duration_cast<seconds>(stop - start);
	debug_file << "Took to execute: " << duration.count() << " seconds." << std::endl;
	std::cout << "Execution has just finished!" << std::endl;
	return 0;
}