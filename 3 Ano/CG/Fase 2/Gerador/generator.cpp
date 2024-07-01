//======================================//
//										//
// Gerador de figuras geometricas 3d.	//
// Computação Gráfica					//
// Universidade do Minho				//
// Paulo Freitas e Ana Silva			//
// 2024									//
//										//
//======================================//

#define _USE_MATH_DEFINES
#include <math.h>
#include <stdio.h>
#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <string.h>
#include <direct.h> 

struct Vertex {
	float x, y, z;
};

enum Model { plane, box, sphere, cone, torus };
std::vector<Vertex> vertices;

void constroiPlane(float lenght, int divisions, const char* file) {

	float comprimento = lenght / divisions;
	float pmedio = lenght / 2;
	float x1, x2;
	float z1, z2;

	for (int i = 0; i < divisions; i++) {
		for (int j = 0; j < divisions; j++) {
			x1 = i * comprimento - pmedio;
			z1 = j * comprimento - pmedio;
			x2 = (i + 1) * comprimento - pmedio;
			z2 = (j + 1) * comprimento - pmedio;

			vertices.push_back({ x1,  0,  z1 });
			vertices.push_back({ x2,  0,  z2 });
			vertices.push_back({ x2,  0,  z1 });


			vertices.push_back({ x1,  0,  z1 });
			vertices.push_back({ x1,  0,  z2 });
			vertices.push_back({ x2,  0,  z2 });
		}
	}

	std::ofstream outfile("..//models//" + (std::string)file);
	std::stringstream texto;
	texto << vertices.size() << "\n";
	for (const auto& v : vertices) {
		texto << v.x << " " << v.y << " " << v.z << "\n";
	}
	outfile << texto.rdbuf();
	outfile.close();

}

void constroiBox(float length, int divisions, const char* file) {

	float comprimento = length / divisions;
	float pmedio = length / 2;
	float x1, x2;
	float y1, y2;
	float z1, z2;

	for (int i = 0; i < divisions; i++) {
		for (int j = 0; j < divisions; j++) {
			x1 = i * comprimento - pmedio;
			y1 = j * comprimento - pmedio;
			x2 = (i + 1) * comprimento - pmedio;
			y2 = (j + 1) * comprimento - pmedio;

			vertices.push_back({ x2, y2, pmedio });
			vertices.push_back({ x1, y1, pmedio });
			vertices.push_back({ x2, y1, pmedio });

			vertices.push_back({ x1, y2, pmedio });
			vertices.push_back({ x1, y1, pmedio });
			vertices.push_back({ x2, y2, pmedio });

			vertices.push_back({ x1, y1, -pmedio });
			vertices.push_back({ x2, y2, -pmedio });
			vertices.push_back({ x2, y1, -pmedio });

			vertices.push_back({ x1, y1, -pmedio });
			vertices.push_back({ x1, y2, -pmedio });
			vertices.push_back({ x2, y2, -pmedio });
		}
	}

	for (int i = 0; i < divisions; i++) {
		for (int j = 0; j < divisions; j++) {
			x1 = i * comprimento - pmedio;
			z1 = j * comprimento - pmedio;
			x2 = (i + 1) * comprimento - pmedio;
			z2 = (j + 1) * comprimento - pmedio;

			vertices.push_back({ x1, pmedio, z1 });
			vertices.push_back({ x2, pmedio, z2 });
			vertices.push_back({ x2, pmedio, z1 });

			vertices.push_back({ x1, pmedio, z1 });
			vertices.push_back({ x1, pmedio, z2 });
			vertices.push_back({ x2, pmedio, z2 });

			vertices.push_back({ x2, -pmedio, z2 });
			vertices.push_back({ x1, -pmedio, z1 });
			vertices.push_back({ x2, -pmedio, z1 });

			vertices.push_back({ x1, -pmedio, z2 });
			vertices.push_back({ x1, -pmedio, z1 });
			vertices.push_back({ x2, -pmedio, z2 });
		}
	}

	for (int i = 0; i < divisions; i++) {
		for (int j = 0; j < divisions; j++) {
			y1 = i * comprimento - pmedio;
			z1 = j * comprimento - pmedio;
			y2 = (i + 1) * comprimento - pmedio;
			z2 = (j + 1) * comprimento - pmedio;

			vertices.push_back({ pmedio, y1, z1 });
			vertices.push_back({ pmedio, y2, z2 });
			vertices.push_back({ pmedio, y1, z2 });

			vertices.push_back({ pmedio, y1, z1 });
			vertices.push_back({ pmedio, y2, z1 });
			vertices.push_back({ pmedio, y2, z2 });

			vertices.push_back({ -pmedio, y2, z2 });
			vertices.push_back({ -pmedio, y1, z1 });
			vertices.push_back({ -pmedio, y1, z2 });

			vertices.push_back({ -pmedio, y2, z1 });
			vertices.push_back({ -pmedio, y1, z1 });
			vertices.push_back({ -pmedio, y2, z2 });
		}
	}
	std::ofstream outfile("..//models//" + (std::string)file);
	std::stringstream texto;
	texto << vertices.size() << "\n";
	for (const auto& v : vertices) {
		texto << v.x << " " << v.y << " " << v.z << "\n";
	}
	outfile << texto.rdbuf();
	outfile.close();
}

void constroiSphere(float raio, int slices, int stacks, const char* file) {

	float x1, x2, x3, x4;
	float y1, y2;
	float z1, z2, z3, z4;
	float arco_alpha = 2 * M_PI / slices;
	float arco_beta = M_PI / stacks;

	for (int i = 0; i < slices; i++) {
		for (int j = 0; j < stacks; j++) {

			x1 = raio * cos(M_PI_2 - arco_beta * j) * sin(arco_alpha * i);
			x2 = raio * cos(M_PI_2 - arco_beta * (j + 1)) * sin(arco_alpha * i);
			x3 = raio * cos(M_PI_2 - arco_beta * (j + 1)) * sin(arco_alpha * (i + 1));
			x4 = raio * cos(M_PI_2 - arco_beta * j) * sin(arco_alpha * (i + 1));

			y1 = raio * sin(M_PI_2 - arco_beta * j);
			y2 = raio * sin(M_PI_2 - arco_beta * (j + 1));

			z1 = raio * cos(M_PI_2 - arco_beta * j) * cos(arco_alpha * i);
			z2 = raio * cos(M_PI_2 - arco_beta * (j + 1)) * cos(arco_alpha * i);
			z3 = raio * cos(M_PI_2 - arco_beta * (j + 1)) * cos(arco_alpha * (i + 1));
			z4 = raio * cos(M_PI_2 - arco_beta * j) * cos(arco_alpha * (i + 1));

			if (j != stacks - 1) {
				vertices.push_back({ x1, y1, z1 });
				vertices.push_back({ x2, y2, z2 });
				vertices.push_back({ x3, y2, z3 });
			}

			if (j != 0) {
				vertices.push_back({ x1, y1, z1 });
				vertices.push_back({ x3, y2, z3 });
				vertices.push_back({ x4, y1, z4 });
			}
		}
	}

	std::ofstream outfile("..//models//" + (std::string)file);
	std::stringstream texto;
	texto << vertices.size() << "\n";
	for (const auto& v : vertices) {
		texto << v.x << " " << v.y << " " << v.z << "\n";
	}
	outfile << texto.rdbuf();
	outfile.close();
}

void constroiCone(float raio, float altura, int slices, int stacks, const char* file) {

	float arco_alpha = 2 * M_PI / slices;
	float ratio = altura / raio;
	float stack_size = altura / stacks;
	float x1, x2, x3, x4;
	float y1, y2;
	float z1, z2, z3, z4;
	float a1, a2;
	float r1, r2;

	for (int i = 1; i < slices; i++) {

		x1 = raio * sin(arco_alpha * i);
		x2 = raio * sin(arco_alpha * (i + 1));

		z1 = raio * cos(arco_alpha * i);
		z2 = raio * cos(arco_alpha * (i + 1));

		vertices.push_back({ x1, 0, z1 });
		vertices.push_back({ 0, 0, 0 });
		vertices.push_back({ x2, 0, z2 });
	}

	for (int i = 0; i < stacks; i++) {
		for (int j = 0; j < slices; j++) {

			a1 = altura - (i * stack_size);
			a2 = altura - ((i + 1) * stack_size);

			r1 = a1 / ratio;
			r2 = a2 / ratio;

			x1 = r1 * sin(arco_alpha * j);
			x2 = r1 * sin(arco_alpha * (j + 1));
			x3 = r2 * sin(arco_alpha * (j + 1));
			x4 = r2 * sin(arco_alpha * j);

			y1 = (i * stack_size);
			y2 = (i + 1) * stack_size;

			z1 = r1 * cos(arco_alpha * j);
			z2 = r1 * cos(arco_alpha * (j + 1));
			z3 = r2 * cos(arco_alpha * (j + 1));
			z4 = r2 * cos(arco_alpha * j);

			vertices.push_back({ x1, y1, z1 });
			vertices.push_back({ x2, y1, z2 });
			vertices.push_back({ x4, y2, z4 });

			if (j != slices - 1) {

				vertices.push_back({ x4, y2, z4 });
				vertices.push_back({ x2, y1, z2 });
				vertices.push_back({ x3, y2, z3 });
			}
		}
	}

	std::ofstream outfile("..//Models//" + (std::string)file);
	std::stringstream texto;
	texto << vertices.size() << "\n";
	for (const auto& v : vertices) {
		texto << v.x << " " << v.y << " " << v.z << "\n";
	}
	outfile << texto.rdbuf();
	outfile.close();

}

void constroiTorus(float raio, float largura, int slices, int stacks, const char* file) {
	std::vector<Vertex> vertices;

	float x1, x2, x3, x4;
	float y1, y2, y3, y4;
	float z1, z2, z3, z4;
	float alfa = (2 * M_PI) / stacks;
	float beta = (2 * M_PI) / slices; 

	float raioM, raiom;
	raioM = (raio + largura) / 2;
	raiom = raio - largura;

	// Gera os vertices  
	for (int i = 0; i < stacks; i++) {
		for (int j = 0; j < slices; j++) {
			x1 = (raioM + raiom * cos(alfa * i)) * cos(beta * j);
			x2 = (raioM + raiom * cos(alfa * (i + 1))) * cos(beta * j);
			x3 = (raioM + raiom * cos(alfa * (i + 1))) * cos(beta * (j + 1));
			x4 = (raioM + raiom * cos(alfa * i)) * cos(beta * (j + 1));

			y1 = raiom * sin(alfa * i);
			y2 = raiom * sin(alfa * (i + 1));
			y3 = raiom * sin(alfa * (i + 1));
			y4 = raiom * sin(alfa * i);

			z1 = (raioM + raiom * cos(alfa * i)) * sin(beta * j);
			z2 = (raioM + raiom * cos(alfa * (i + 1))) * sin(beta * j);
			z3 = (raioM + raiom * cos(alfa * (i + 1))) * sin(beta * (j + 1));
			z4 = (raioM + raiom * cos(alfa * i)) * sin(beta * (j + 1));

			vertices.push_back({ x1,  y1,  z1 });
			vertices.push_back({ x2,  y2,  z2 });
			vertices.push_back({ x4,  y4,  z4 });

			vertices.push_back({ x2,  y2,  z2 });
			vertices.push_back({ x3,  y3,  z3 });
			vertices.push_back({ x4,  y4,  z4 });
		}
	}
	std::ofstream outfile("..//models//" + (std::string)file);
	std::stringstream texto;
	texto << vertices.size() << "\n";
	for (const auto& v : vertices) {
		texto << v.x << " " << v.y << " " << v.z << "\n";
	}
	outfile << texto.rdbuf();
	outfile.close();
}


int forma(int argc, char* argv[]) {
	int x;
	if (argc <= 1) x = -1;
	else if (strcmp((argv[1]), "plane") == 0 && argc == 5) x = plane;
	else if (strcmp((argv[1]), "box") == 0 && argc == 5) x = box;
	else if (strcmp((argv[1]), "sphere") == 0 && argc == 6) x = sphere;
	else if (strcmp((argv[1]), "cone") == 0 && argc == 7) x = cone;
	else if (strcmp((argv[1]), "torus") == 0 && argc == 7) x = torus;
	else x = -1;

	return x;
}

int main(int argc, char** argv) {

	if (argc < 3) {
		std::cout << "Falta argumentos" << std::endl;
		return -1;
	}

	int opcao = forma(argc, argv);

	if (opcao == -1) {
		printf("Modelo invalido ou falta de argumentos.\n");
		printf("(plane, box, sphere, cone.)\n\n");
		return -1;
	}
	printf("Comecando processamento... ");

	const char* file = argv[argc - 1];

	switch (opcao)
	{
	case plane:
	{
		float lenght = std::stof(argv[2]);
		int divisions = std::stoi(argv[3]);

		constroiPlane(lenght, divisions, file);
		opcao = 0;
		break;
	}
	case box:
	{
		float lenght = std::stof(argv[2]);
		int divisions = std::stoi(argv[3]);
		constroiBox(lenght, divisions, file);
		opcao = 0;
		break;
	}
	case sphere:
	{
		float raio = std::stof(argv[2]);
		int slices = std::stoi(argv[3]);
		int stacks = std::stoi(argv[4]);
		constroiSphere(raio, slices, stacks, file);
		opcao = 0;
		break;
	}

	case cone:
	{
		float raio = std::stof(argv[2]);
		float altura = std::stof(argv[3]);
		int slices = std::stoi(argv[4]);
		int stacks = std::stoi(argv[5]);
		constroiCone(raio, altura, slices, stacks, file);
		opcao = 0;
		break;
	}
	case torus:
		float raio = std::stof(argv[2]);
		float largura = std::stof(argv[3]);
		int slices = std::stoi(argv[4]);
		int stacks = std::stoi(argv[5]);
		constroiTorus(raio, largura, slices, stacks, file);
		opcao = 0;
		break;
	}

	if (opcao == 0) {
		printf("Feito\n");
	}

	return 0;
}