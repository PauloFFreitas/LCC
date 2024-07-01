//======================================//
//										//
// Gerador de figuras geometricas 3d.	//
// Fase 2                               //
// Computação Gráfica					//
// Universidade do Minho				//
// Ana Silva e Paulo Freitas  			//
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

// diretorio ficheiros de modelos
std::string pmodels;
// diretorio ficheiros de xml
std::string pxml;

// Classe interface entre as possiveis transforacoes
class Transformation {
public:
    void virtual apply() = 0;
};

// Classe de Translacao
class Translate : public Transformation {
    float x, y, z;
public:
    Translate(float x, float y, float z) {
        this->x = x;
        this->y = y;
        this->z = z;
    }
    void apply() {
        glTranslated(x, y, z);
    }
};

// Classe de Rotacao
class Rotate : public Transformation {
    float x, y, z, angle;
public:
    Rotate(float angle, float x, float y, float z) {
        this->angle = angle;
        this->x = x;
        this->y = y;
        this->z = z;
    }
    void apply() {
        glRotatef(angle, x, y, z);
    }
};

// Classe de Escala
class Scale : public Transformation {
    float x, y, z;
public:
    Scale(float x, float y, float z) {
        this->x = x;
        this->y = y;
        this->z = z;
    }
    void apply() {
        glScalef(x, y, z);
    }
};

//Struck do modelo em vetores de pontos e transformacoes
struct Model {
    std::vector<Ponto> points;                                          // Vetor com os pontos do modelo
    std::vector<Transformation*> transformations;                       // Vetor com os pointers das transformacoes do xml

    Model(std::vector<Ponto> p, std::vector<Transformation*> t) {       // Recebe o vetor de pontos e o vetor de tranformaçoes e armazena a informacao na struck
        this->points = p;
        this->transformations = t;
    }
};

// Vetor com os modelos
std::vector<Model> models;

// Inicializacao de vars
float alpha = 0.0f, beta = 0.0f, radius = 5.0f;
float positionX, positionY, positionZ;
float lookAtX = 0.0, lookAtY = 0.0, lookAtZ = 0.0;
float upX = 0.0, upY = 1.0, upZ = 0.0;
float fov = 45.0f, perto = 1.0f, longe = 1000.0f;
int windowX = 0, windowY = 0;

// Recebe o nome do arquivo como parametro e retorna o vetor de pontos que formam o modelo 3D
std::vector<Ponto> getModel(std::string arquivo) {
    std::ifstream file_input(arquivo);
    file_input.ignore(10000, '\n');                                 //skip da primeira linha
    float x, y, z;
    std::vector<Ponto> model;

    while (file_input >> x >> y >> z) {
        model.push_back(Ponto(x, y, z));
    }
    file_input.close();
    return model;

}

// Le os grupos do xml e processa os diferentes comandos mediante palavras chaves
void readGroup(tinyxml2::XMLElement* group, std::vector<Transformation*> ts) {
    using namespace tinyxml2;
    std::vector<Transformation*> backup = ts;

    while (group) {
        ts = backup;
        XMLElement* transformation = group->FirstChildElement("transform");

        if (transformation) {
            for (XMLElement* t = transformation->FirstChildElement(); t; t = t->NextSiblingElement()) {
                std::string name = std::string(t->Name());

                if (name == "translate") {
                    float x, y, z;
                    x = atof(t->Attribute("x"));
                    y = atof(t->Attribute("y"));
                    z = atof(t->Attribute("z"));

                    ts.push_back(new Translate(x, y, z));
                }
                else if (name == "rotate") {
                    float x, y, z, angle;
                    angle = atof(t->Attribute("angle"));
                    x = atof(t->Attribute("x"));
                    y = atof(t->Attribute("y"));
                    z = atof(t->Attribute("z"));

                    ts.push_back(new Rotate(angle, x, y, z));
                }
                else if (name == "scale") {
                    float x, y, z;
                    x = atof(t->Attribute("x"));
                    y = atof(t->Attribute("y"));
                    z = atof(t->Attribute("z"));

                    ts.push_back(new Scale(x, y, z));
                }
                else {
                    std::cout << "error: incorrect syntax" << std::endl;
                }
            }
        }

        XMLElement* MODELS = group->FirstChildElement("models");

        if (MODELS) {
            for (XMLElement* modelo = MODELS->FirstChildElement("model"); modelo; modelo = modelo->NextSiblingElement()) {
                std::vector<Ponto> points = getModel(pmodels + modelo->Attribute("file"));

                models.push_back(Model(points, ts));
            }
        }

        readGroup(group->FirstChildElement("group"), ts);
        group = group->NextSiblingElement("group");
    }
}

// Le o ficheiro xml as informacoes acerca da camara e janela
void readXml(std::string source) {
    using namespace tinyxml2;
    
    printf("Abrindo o ficheiro...");
    XMLDocument doc;
    doc.LoadFile(source.data());
    printf("Feito\n");

    // le tamanho da janela
    XMLElement* window = doc.FirstChildElement("world")->FirstChildElement("window");
    windowX = atoi(window->Attribute("width"));
    windowY = atoi(window->Attribute("height"));
    
    // le posicao da carmara
    XMLElement* camera = doc.FirstChildElement("world")->FirstChildElement("camera");
    XMLElement* position = camera->FirstChildElement("position");
    positionX = atof(position->Attribute("x"));
    positionY = atof(position->Attribute("y"));
    positionZ = atof(position->Attribute("z"));
    
    // le para onde a camara olha
    XMLElement* lookAt = camera->FirstChildElement("lookAt");
    lookAtX = atof(lookAt->Attribute("x"));
    lookAtY = atof(lookAt->Attribute("y"));
    lookAtZ = atof(lookAt->Attribute("z"));

    // le o vetor up
    XMLElement* up = camera->FirstChildElement("up");
    upX = atof(up->Attribute("x"));
    upY = atof(up->Attribute("y"));
    upZ = atof(up->Attribute("z"));

    // le configs de projecao
    XMLElement* projection = camera->FirstChildElement("projection");
    fov = atof(projection->Attribute("fov"));
    perto = atof(projection->Attribute("near"));
    longe = atof(projection->Attribute("far"));

    radius = sqrt(positionX * positionX + positionY * positionY + positionZ * positionZ);
    alpha = asin(positionX/(radius * cos(beta)));
    beta = asin(positionY/radius);
    
    // le os grupos
    XMLElement* group = doc.FirstChildElement("world")->FirstChildElement("group");
    std::vector<Transformation*> t;
    readGroup(group, t);
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

//desenha as figuras e processa as transforamoes indicadas no vetor models
void drawModels() {
    glColor3f(1.0f, 1.0f, 1.0f);
    for (Model model : models) {
        glPushMatrix();
        for (Transformation* t : model.transformations) {
            t->apply();
        }

        // desenha
        glBegin(GL_TRIANGLES);
        for (Ponto p : model.points) {
            glVertex3f(p.x, p.y, p.z);
        }
        glEnd();
        glPopMatrix();
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

    // Diretorios dos modelos (.3d) e xml e (.xml)
    pmodels = "..//Models//";
    pxml = "..//Xml//";

    posCamara();
    
    if (argc == 2)
        readXml(pxml + argv[1]);
    
    // Comandos do glut para inicializar, tamanho de window etc
    glutInit(&argc, argv);
    glutInitDisplayMode(GLUT_DEPTH | GLUT_DOUBLE | GLUT_RGBA);
    glutInitWindowPosition(100, 100);
    glutInitWindowSize(800, 800);
    glutCreateWindow("ProjetoCG");

    // Funcoes de atualizacao
    glutDisplayFunc(renderScene);
    glutReshapeFunc(changeSize);

    // Teclas precionadas
    glutSpecialFunc(keyboardFunc);

    // Definicoes do OpenGL
    glEnable(GL_DEPTH_TEST);
    glEnable(GL_CULL_FACE);

    // Ciclo
    glutMainLoop();
    
    return 1;
}