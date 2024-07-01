//======================================//
//										//
// Gerador de figuras geometricas 3d.	//
// Computação Gráfica					//
// Universidade do Minho				//
// Paulo Freitas e Ana Silva			//
// 2024									//
//										//
//======================================//

#include <vector>
#ifdef __APPLE__
#include <GLUT/glut.h>
#else
#include <GL/glut.h>
#endif
#include <math.h>
#include <iostream>
#include <fstream>

#include "tinyxml2.h"

// struck de um ponto no espaço com as suas variaveis x y z
struct Ponto {
    float x, y, z;
    Ponto(float x, float y, float z) {
        this->x = x;
        this->y = y;
        this->z = z;
    }
};

std::string pmodels;
std::string pxml;

// vetor de vetores de Pontos obtidos apartir dos arquivos .3d
std::vector<std::vector<Ponto>> models;

// inicializacao de vars
float alpha = 0.0f, beta = 0.0f, radius = 5.0f;
float positionX, positionY, positionZ;
float lookAtX = 0.0, lookAtY = 0.0, lookAtZ = 0.0;
float upX = 0.0, upY = 1.0, upZ = 0.0;
float fov = 45.0f, perto = 1.0f, longe = 1000.0f;
int windowX = 0, windowY = 0;

// le os pontos armazenados no .3d e guarda no vetor de vetores
void readModel(std::string source) {
    std::ifstream file_input(source);
    file_input.ignore(10000, '\n'); // skip da primeira linha
    float x, y, z;
    std::vector<Ponto> model;

    while (file_input >> x >> y >> z) {
        model.push_back(Ponto(x, y, z));
    }
    models.push_back(model);
    file_input.close();
}

// le o ficheiro xml e analisa cada instrucao e calcula novas vars
void readXml(std::string source) {

    using namespace tinyxml2;

    XMLDocument doc;
    doc.LoadFile(source.data());

    XMLElement* window = doc.FirstChildElement("world")->FirstChildElement("window");
    windowX = atoi(window->Attribute("width"));
    windowY = atoi(window->Attribute("height"));

    XMLElement* camera = doc.FirstChildElement("world")->FirstChildElement("camera");
    XMLElement* position = camera->FirstChildElement("position");
    positionX = atof(position->Attribute("x"));
    positionY = atof(position->Attribute("y"));
    positionZ = atof(position->Attribute("z"));

    XMLElement* lookAt = camera->FirstChildElement("lookAt");
    lookAtX = atof(lookAt->Attribute("x"));
    lookAtY = atof(lookAt->Attribute("y"));
    lookAtZ = atof(lookAt->Attribute("z"));

    XMLElement* up = camera->FirstChildElement("up");
    upX = atof(up->Attribute("x"));
    upY = atof(up->Attribute("y"));
    upZ = atof(up->Attribute("z"));

    XMLElement* projection = camera->FirstChildElement("projection");
    fov = atof(projection->Attribute("fov"));
    perto = atof(projection->Attribute("near"));
    longe = atof(projection->Attribute("far"));

    XMLElement* mod = doc.FirstChildElement("world")->FirstChildElement("group")->FirstChildElement("models");
    XMLElement* model = mod->FirstChildElement("model");

    while (model) {
        std::string model_path = model->Attribute("file");
        readModel(pmodels + model_path);
        model = model->NextSiblingElement("model");
    }

    radius = sqrt(positionX * positionX + positionY * positionY + positionZ * positionZ);
    alpha = asin(positionX / (radius * cos(beta)));
    beta = asin(positionY / radius);
}

void changeSize(int w, int h) {

    // se o xml tiver indicacoes de janela:
    if (windowX != 0 || windowY != 0) {
        w = windowX;
        h = windowY;
    }

    // evitar divisao por zero quando janela é pequena
    if (h == 0) h = 1;

    // alterar o tamanho da janela para igual ao .xml
    glutReshapeWindow(w, h);

    // ratio da janela 
    float ratio = w * 1.0 / h;

    // define a matriz projecao 
    glMatrixMode(GL_PROJECTION);
    // carrega a matriz identidade
    glLoadIdentity();

    // define o viewport
    glViewport(0, 0, w, h);

    // define prespectiva
    gluPerspective(fov, ratio, perto, longe);

    // retorna para o modo de modelview
    glMatrixMode(GL_MODELVIEW);
}

// desenha os eixos x y z
void drawAxis() {

    glBegin(GL_LINES);
    glColor3f(1.0, 0.0, 0.0);
    glVertex3f(-radius, 0, 0);
    glVertex3f(radius, 0, 0);

    glColor3f(0.0, 1.0, 0.0);
    glVertex3f(0, -radius, 0);
    glVertex3f(0, radius, 0);

    glColor3f(0, 0, 1.0);
    glVertex3f(0, 0, -radius);
    glVertex3f(0, 0, radius);

    glEnd();
    glColor3f(1.0, 1.0, 1.0);
}

// com base nos pontos no vetor, desenha
void drawModels() {
    glColor3f(1.0f, 1.0f, 1.0f);
    for (std::vector<Ponto> model : models) {
        glBegin(GL_TRIANGLES);
        for (Ponto p : model) {
            glVertex3f(p.x, p.y, p.z);
        }
        glEnd();
    }
}

void renderScene(void) {
    // clear buffers
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

    // set the camera
    glLoadIdentity();
    gluLookAt(positionX, positionY, positionZ, lookAtX, lookAtY, lookAtZ, upX, upY, upZ);

    glPolygonMode(GL_FRONT, GL_LINE);

    drawAxis();
    drawModels();

    // End of frame
    glutSwapBuffers();
}

//calculo da posicao da camera
void posCamara() {
    positionX = radius * cos(beta) * sin(alpha);
    positionY = radius * sin(beta);
    positionZ = radius * cos(beta) * cos(alpha);
}

void keyboardFunc(int key, int xx, int yy) {

    switch (key) {

    case GLUT_KEY_UP: // cima
        beta += 0.1f;
        if (beta > 1.5f)
            beta = 1.5f;
        break;

    case GLUT_KEY_RIGHT: // direita
        alpha -= 0.1;
        break;

    case GLUT_KEY_LEFT: // esquerda
        alpha += 0.1;
        break;

    case GLUT_KEY_DOWN: // baixo
        beta -= 0.1f;
        if (beta < -1.5f)
            beta = -1.5f;
        break;

    case GLUT_KEY_PAGE_UP: // zoom in
        radius -= 0.1f;
        if (radius < 0.1f)
            radius = 0.1f;
        break;

    case GLUT_KEY_PAGE_DOWN: // zoom out
        radius += 0.1f;
        break;

    default:
        break;
    }
    posCamara();
    glutPostRedisplay();

}

int main(int argc, char** argv) {
    // diretorios dos ´.3d e .xml
    pmodels = "..//Models//";
    pxml = "..//Xml//";

    posCamara();

    if (argc == 2)
        readXml(pxml + argv[1]);

    // comandos do glut para inicializar, tamanho de window etc
    glutInit(&argc, argv);
    glutInitDisplayMode(GLUT_DEPTH | GLUT_DOUBLE | GLUT_RGBA);
    glutInitWindowPosition(100, 100);
    glutInitWindowSize(800, 800);
    glutCreateWindow("ProjetoCG");

    // Funcoes de atualizacao
    glutDisplayFunc(renderScene);
    glutReshapeFunc(changeSize);

    // teclas precionadas
    glutSpecialFunc(keyboardFunc);

    // definicoes do OpenGL
    glEnable(GL_DEPTH_TEST);
    glEnable(GL_CULL_FACE);

    // ciclo
    glutMainLoop();

    return 1;
}