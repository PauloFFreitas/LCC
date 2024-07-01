//======================================//
//										//
// Engine sistema solar.            	//
// Fase 4                               //
// Computação Gráfica					//
// Universidade do Minho				//
// Ana Silva e Paulo Freitas  			//
// 2024									//
//										//
//======================================//

#include <IL/il.h>
#include <vector>
#ifdef __APPLE__
#include <GLUT/glut.h>
#else
#include <GL/glew.h>
#include <GL/glut.h>
#include <GL/gl.h>
#include <GL/glu.h>
#endif
#include <math.h>
#include <iostream>
#include <fstream>
#include "tinyxml2.h"
#include "luzes.h"
#include <map>

// Inicializacao de vars
float alpha = 0.0f, beta = 0.0f, radius = 5.0f;
float positionX, positionY, positionZ;
float lookAtX = 0.0, lookAtY = 0.0, lookAtZ = 0.0; // definiçoes gerais, inicializa com estas caso nao especificado
float upX = 0.0, upY = 1.0, upZ = 0.0;
float fov = 45.0f, perto = 1.0f, longe = 1000.0f;
int windowX, windowY;
int speed = 1;

// diretorio ficheiros de modelos
std::string pmodels;
// diretorio ficheiros de xml
std::string pxml;
// diretorio ficheiros de texturas
std::string ptextura;

// struck de um ponto no espaço com as suas variaveis x y z
struct Ponto {
    float x, y, z;
    Ponto(float x, float y, float z) {
        this->x = x;
        this->y = y;
        this->z = z;
    }
};
// struct da normal x,y,z
struct Normal {
    float nx, ny, nz;
    Normal(float nx, float ny, float nz) {
        this->nx = nx;
        this->ny = ny;
        this->nz = nz;
    }
};
// coordenadas de textura
struct TexturaC {
    float tx, tz;
    TexturaC(float tx, float tz) {
        this->tx = tx;
        this->tz = tz;
    }
};

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
// Class de Rotacao sobre angulo
class RotateAngle : public Transformation {
    float x, y, z, angle;
public:
    RotateAngle(float angle, float x, float y, float z) {
        this->angle = angle;
        this->x = x;
        this->y = y;
        this->z = z;
    }

    void apply() {
        glRotatef(angle, x, y, z);
    }
};
// Class de Rotacao sobre tempo
class RotateTime : public Transformation {
    float x, y, z, time;
public:
    RotateTime(float time, float x, float y, float z) {
        this->time = time* 1440;
        this->x = x;
        this->y = y;
        this->z = z;
    }

    void apply() {
        float angle = glutGet(GLUT_ELAPSED_TIME) * 360 / (time/speed);
        glRotatef(angle, x, y, z);
    }
};

// AUXILIARES CURVAS
// conversao de ponto par array
void convertPonto(Ponto p, float* point){
    point[0] = p.x;
    point[1] = p.y;
    point[2] = p.z;

}

//contruir 4x4 matriz de rotaçao
void buildRotMatrix(float* x, float* y, float* z, float* m) {
    int idx = 0;
    for (int i = 0; i < 3; i++) {
        m[idx++] = x[i];
    }
    m[idx++] = 0;
    for (int i = 0; i < 3; i++) {
        m[idx++] = y[i];
    }
    m[idx++] = 0;
    for (int i = 0; i < 3; i++) {
        m[idx++] = z[i];
    }
    m[idx++] = 0;
    m[idx++] = 0;
    m[idx++] = 0;
    m[idx++] = 0;
    m[idx] = 1;
}

// produto vetorial
void cruzamento(float* a, float* b, float* res) {
    res[0] = a[1] * b[2] - a[2] * b[1];
    res[1] = a[2] * b[0] - a[0] * b[2];
    res[2] = a[0] * b[1] - a[1] * b[0];
}

// normaliza vetor
void normalize(float* a) {
    float norm = sqrt(a[0] * a[0] + a[1] * a[1] + a[2] * a[2]);
    a[0] = a[0] / norm;
    a[1] = a[1] / norm;
    a[2] = a[2] / norm;
}

// produto matriz x vetor
void multMatrixVector(float* m, float* v, float* res) {
    for (int j = 0; j < 4; ++j) {
        res[j] = 0;
        for (int k = 0; k < 4; ++k) {
            res[j] += v[k] * m[j * 4 + k];
        }
    }
}

//calcula a posicao e derivada Catmull-Rom no ponto t
void getCatmullRomPoint(float t, float* p0, float* p1, float* p2, float* p3, float* pos, float* deriv) {
    //matriz catmull-rom 
    float m[4][4] = {
        {-0.5f,  1.5f, -1.5f,  0.5f},
        { 1.0f, -2.5f,  2.0f, -0.5f},
        {-0.5f,  0.0f,  0.5f,  0.0f},
        { 0.0f,  1.0f,  0.0f,  0.0f}
    };

    for (int i = 0; i < 3; i++) {
        float A[4];
        float temp[] = { p0[i],p1[i],p2[i],p3[i] };
        multMatrixVector((float*)m, temp, A);


        pos[i] = pow(t, 3) * A[0] + pow(t, 2) * A[1] + t * A[2] + A[3];
        deriv[i] = 3 * pow(t, 2) * A[0] + 2 * t * A[1] + A[2];
    }
}

//calculo ponto global
void getGlobalCatmullRomPoint(float gt, float* pos, float* deriv, std::vector<Ponto> p) {
    int POINT_COUNT = p.size();

    float t = gt * POINT_COUNT;
    int index = floor(t);
    t = t - index;

    //indices guarda os pontos
    int indices[4];
    indices[0] = (index + POINT_COUNT - 1) % POINT_COUNT;
    indices[1] = (indices[0] + 1) % POINT_COUNT;
    indices[2] = (indices[1] + 1) % POINT_COUNT;
    indices[3] = (indices[2] + 1) % POINT_COUNT;

    float p0[3], p1[3], p2[3], p3[3];
    convertPonto(p[indices[0]], p0);
    convertPonto(p[indices[1]], p1);
    convertPonto(p[indices[2]], p2);
    convertPonto(p[indices[3]], p3);

    getCatmullRomPoint(t, p0, p1, p2, p3, pos, deriv);
}

// desenha curva Catmull-Rom
void renderCatmullRomCurve(std::vector<Ponto> control_points) {
    float pos[3], deriv[3];
    float LINE_SEGMENTS = 100;

    glBegin(GL_LINE_LOOP);
    for (int i = 0; i < LINE_SEGMENTS; i++) {
        getGlobalCatmullRomPoint(1 / LINE_SEGMENTS * i, pos, deriv, control_points);
        glVertex3f(pos[0], pos[1], pos[2]);
    }
    glEnd();
}

// Class de Cruva
class Curve : public Transformation {
    float t, time, current_time;
    std::vector<Ponto> control_points;
    bool align;
    float prev_y[3];
public:
    Curve(std::vector<Ponto> points, bool align, float time) {
        this->t = 0;
        this->control_points = points;
        this->time = time * 1440;
        this->current_time = 0;
        this->align = align;
        this->prev_y[0] = 0;
        this->prev_y[1] = 1;
        this->prev_y[2] = 0;
    }

    void apply() {
        float pos[3], deriv[3];
        renderCatmullRomCurve(this->control_points);
        getGlobalCatmullRomPoint(t, pos, deriv, control_points);

        glTranslatef(pos[0], pos[1], pos[2]);
        if (align) {
            float x[3] = { deriv[0],deriv[1],deriv[2] };
            float y[3];
            float z[3];
            float m[16];

            normalize(x);
            cruzamento(x, prev_y, z);
            normalize(z);
            cruzamento(z, x, y);

            normalize(y);
            memcpy(prev_y, y, 3 * sizeof(float));

            buildRotMatrix(x, y, z, m);
            glMultMatrixf(m);
        }
        float new_time = glutGet(GLUT_ELAPSED_TIME);
        float diff = new_time - current_time;

        t += diff / (time/speed);
        current_time = new_time;
    }
};

//Struck do modelo em vetores de pontos e transformacoes
struct Model {
    
    std::vector<Ponto> points;                                          // Vetor com os pontos do modelo
    std::vector<Transformation*> transformations;                       // Vetor com os pointers das transformacoes do xml
    std::vector<Color*> colors;
    std::vector<Normal> normals;
    std::vector<TexturaC> texturasc;
    GLuint texture;

    Model(std::vector<Ponto> p, std::vector<Normal> n, std::vector<TexturaC> texc, std::vector<Transformation*> t, std::vector<Color*> c, GLuint tex) {
        this->points = p;
        this->normals = n;
        this->texturasc = texc;
        this->transformations = t;
        this->colors = c;
        this->texture = tex;
    }
};

// Vetor com os modelos
std::vector<Model> models;
// vetor de luzes
std::vector<Light*> lights;

//Recebe o nome do arquivo como parametro e retorna o vetor de pontos que formam o modelo 3D
std::vector<Ponto> getModelPontos(std::string source) {
    std::ifstream file_input(source);
    file_input.ignore(10000, '\n'); // skip da primeira linha
    float x, y, z, nx, ny, nz, tx, tz;
    //GLuint vertexCount1, vertices1, normals1, texCoord1;

    std::vector<Ponto> model;

    while (file_input >> x >> y >> z >> nx >> ny >> nz >> tx >> tz) {
        model.push_back(Ponto(x, y, z));
    }
    file_input.close();
    return model;
}

//Recebe o ficheiro e guarda as coordenadas normais
std::vector<Normal> getModelNormais(std::string source) {
    std::ifstream file_input(source);
    file_input.ignore(10000, '\n'); // skip da primeira linha
    float x, y, z, nx, ny, nz, tx, tz;

    std::vector<Normal> model;

    while (file_input >> x >> y >> z >> nx >> ny >> nz >> tx >> tz) {
        model.push_back(Normal(nx, ny, nz));
    }
    file_input.close();
    return model;
}

//Recebe o ficheiro e guarda as coordernadas textura
std::vector<TexturaC> getModelTexturaC(std::string source) {
    std::ifstream file_input(source);
    file_input.ignore(10000, '\n'); // skip da primeira linha
    float x, y, z, nx, ny, nz, tx, tz;

    std::vector<TexturaC> model;

    while (file_input >> x >> y >> z >> nx >> ny >> nz >> tx >> tz) {
        model.push_back(TexturaC(tx, tz));
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
                    if (t->Attribute("time") == nullptr) {
                        float x, y, z;
                        x = atof(t->Attribute("x"));
                        y = atof(t->Attribute("y"));
                        z = atof(t->Attribute("z"));

                        ts.push_back(new Translate(x, y, z));
                    }
                    else { // tem informacao de tempo
                        float time;
                        std::string align;
                        std::vector<Ponto> curve;
                        time = (atof(t->Attribute("time")));
                        align = t->Attribute("align");

                        for (XMLElement* p = t->FirstChildElement("point"); p; p = p->NextSiblingElement("point")) {
                            float x, y, z;
                            x = atof(p->Attribute("x"));
                            y = atof(p->Attribute("y"));
                            z = atof(p->Attribute("z"));

                            curve.push_back(Ponto(x, y, z));
                        }
                        ts.push_back(new Curve(curve, align == "True", time));
                    }
                }
                else if (name == "rotate") {
                    if (t->Attribute("time") == nullptr) {
                        float x, y, z, angle;
                        angle = atof(t->Attribute("angle"));
                        x = atof(t->Attribute("x"));
                        y = atof(t->Attribute("y"));
                        z = atof(t->Attribute("z"));

                        ts.push_back(new RotateAngle(angle, x, y, z));
                    }
                    else { // tem informacao de tempo
                        float x, y, z, time;
                        time = (atof(t->Attribute("time")));
                        x = atof(t->Attribute("x"));
                        y = atof(t->Attribute("y"));
                        z = atof(t->Attribute("z"));

                        ts.push_back(new RotateTime(time, x, y, z));
                    }
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
                int texturaid = -1;
                std::vector<Ponto> points = getModelPontos(pmodels + modelo->Attribute("file"));
                std::vector<Normal> normais = getModelNormais(pmodels + modelo->Attribute("file"));
                std::vector<TexturaC> texturaC = getModelTexturaC(ptextura + modelo->Attribute("file"));

                std::vector<Color*> colors;
                XMLElement* COLORS = modelo->FirstChildElement("color");
                if (COLORS) {
                    for (XMLElement* COLOR = COLORS->FirstChildElement(); COLOR; COLOR = COLOR->NextSiblingElement()) {
                        std::string ColorName = std::string(COLOR->Name());
                        printf("Color element found: %s\n", ColorName.c_str());

                        if (ColorName == "diffuse") {
                            float r = atof(COLOR->Attribute("R")) / 255;
                            float rg = atof(COLOR->Attribute("G")) / 255;
                            float rgb = atof(COLOR->Attribute("B")) / 255;

                            colors.push_back(new Diffuse(r, rg, rgb));
                        }
                        else if (ColorName == "ambient") {
                            float r = atof(COLOR->Attribute("R")) / 255;
                            float rg = atof(COLOR->Attribute("G")) / 255;
                            float rgb = atof(COLOR->Attribute("B")) / 255;

                            colors.push_back(new Ambient(r, rg, rgb));
                        }
                        else if (ColorName == "specular") {
                            float r = atof(COLOR->Attribute("R")) / 255;
                            float rg = atof(COLOR->Attribute("G")) / 255;
                            float rgb = atof(COLOR->Attribute("B")) / 255;

                            colors.push_back(new Specular(r, rg, rgb));
                        }
                        else if (ColorName == "emissive") {
                            float r = atof(COLOR->Attribute("R")) / 255;
                            float rg = atof(COLOR->Attribute("G")) / 255;
                            float rgb = atof(COLOR->Attribute("B")) / 255;

                            colors.push_back(new Emissive(r, rg, rgb));
                        }
                        else if (ColorName == "shininess") {
                            float value = atof(COLOR->Attribute("value"));

                            colors.push_back(new Shininess(value));
                        }
                    }
                }
                models.push_back(Model(points, normais, texturaC, ts, colors, texturaid));
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
    alpha = asin(positionX / (radius * cos(beta)));
    beta = asin(positionY / radius);

    XMLElement* LIGHTS = doc.FirstChildElement("world")->FirstChildElement("lights");

    int lightIndex = 0;
    if (LIGHTS) {
        for (XMLElement* LIGHT = LIGHTS -> FirstChildElement("light"); LIGHT; LIGHT = LIGHT -> NextSiblingElement("light")) {
            std::string lightType = LIGHT->Attribute("type");
            if (lightType == "point") {
                printf("point\n");
                float x = atof(LIGHT ->Attribute("posx"));
                float y = atof(LIGHT ->Attribute("posy"));
                float z = atof(LIGHT ->Attribute("posz"));

                lights.push_back(new LightPoint(x, y, z, lightIndex));
            }
            else if (lightType == "directional") {
                float dirx = atof(LIGHT->Attribute("dirx"));
                float diry = atof(LIGHT->Attribute("diry"));
                float dirz = atof(LIGHT->Attribute("dirz"));

                lights.push_back(new LightDirectional(dirx, diry, dirz, lightIndex));
            }
            else if (lightType == "spot") {
                float x = atof(LIGHT->Attribute("posx"));
                float y = atof(LIGHT->Attribute("posy"));
                float z = atof(LIGHT->Attribute("posz"));
                float dirx = atof(LIGHT->Attribute("dirx"));
                float diry = atof(LIGHT->Attribute("diry"));
                float dirz = atof(LIGHT->Attribute("dirz"));
                float cutoff = atof(LIGHT->Attribute("cutoff"));

                lights.push_back(new LightSpotlight(x, y, z, dirx, diry, dirz, cutoff, lightIndex));
            }
            lightIndex++;
        }
    }
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

    glDisable(GL_LIGHTING);
    glBegin(GL_LINES);
    glColor3f(1.0, 0.0, 0.0);
    glVertex3f(-radius, 0, 0);
    glVertex3f(radius, 0, 0);

    glColor3f(0.0, 1.0, 0.0);
    glVertex3f(0, -radius, 0);
    glVertex3f(0, radius, 0);

    glColor3f(0, 0, 1);
    glVertex3f(0, 0, -radius);
    glVertex3f(0, 0, radius);

    glEnd();
    glColor3f(1.0, 1.0, 1.0);
    glEnable(GL_LIGHTING);
}

// Aplica as luzes
void lightsOn() {
    for (Light* l : lights) {
        l->apply();
    }
}

//desenha as figuras e processa as transforamoes indicadas no vetor models
void drawModels() {
    glColor3f(1.0f, 1.0f, 1.0f);

    for (Model model : models) {
        glPushMatrix();
        for (Transformation* t : model.transformations) {
            t->apply();
        }

        for (Color* c : model.colors) {
            c->apply();
        }

        glBegin(GL_TRIANGLES);
        for (Ponto p : model.points) {
            glVertex3f(p.x, p.y, p.z);
        }
        for (Normal n : model.normals) {
            glNormal3f(n.nx, n.ny, n.nz);
        }
        if (model.texture != -1) {
            for (TexturaC tex : model.texturasc) {
                glTexCoord2f(tex.tx, tex.tz);
            }
        }

        glEnd();

        float clear[4] = { 0.0f, 0.0f, 0.0f, 1.0f };
        glMaterialfv(GL_FRONT, GL_EMISSION, clear);

        glBindTexture(GL_TEXTURE_2D, 0);
        glPopMatrix();
    }
}

void renderScene(void) {
    // clear buffers
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

    // set the camera
    glLoadIdentity();
    gluLookAt(positionX, positionY, positionZ, lookAtX, lookAtY, lookAtZ, upX, upY, upZ);

    //glPolygonMode(GL_FRONT, GL_FILL);
    
    //drawAxis();
    lightsOn();
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
        beta += 0.05f;
        if (beta > 1.57f)
            beta = 1.57f;
        break;

    case GLUT_KEY_RIGHT: // direita
        alpha += 0.05;
        break;

    case GLUT_KEY_LEFT: // esquerda
        alpha -= 0.05;
        break;

    case GLUT_KEY_DOWN: // baixo
        beta -= 0.05f;
        if (beta < -1.57f)
            beta = -1.57f;
        break;

    case GLUT_KEY_PAGE_UP: // zoom in
        radius -= 5.0f;
        if (radius < 0.1f)
            radius = 0.1f;
        break;

    case GLUT_KEY_PAGE_DOWN: // zoom out
        radius += 5.0f;
        break;

    case GLUT_KEY_F1:
        speed = 1;
        std::cout << "Speed: " << speed << "x" << std::endl;
        break;
    case GLUT_KEY_F2:
        speed = 2;
        std::cout << "Speed: " << speed << "x" << std::endl;
        break;
    case GLUT_KEY_F3:
        speed = 4;
        std::cout << "Speed: " << speed << "x" << std::endl;
        break;
    case GLUT_KEY_F4:
        speed = 8;
        std::cout << "Speed: " << speed << "x" << std::endl;
        break;
    case GLUT_KEY_F5:
        speed = 16;
        std::cout << "Speed: " << speed << "x" << std::endl;
        break;

    default:
        break;
    }
    posCamara();
    glutPostRedisplay();

}

int main(int argc, char** argv) {

    // Diretorios dos modelos (.3d) e xml (.xml)
    pmodels = "..//Models//";
    pxml = "..//Xml//";
    ptextura = "..//Texturas//";

    // Comandos do glut para inicializar, tamanho de window etc
    glutInit(&argc, argv);
    glutInitDisplayMode(GLUT_DEPTH | GLUT_DOUBLE | GLUT_RGBA);
    glutInitWindowPosition(100, 100);
    glutInitWindowSize(800, 800);
    glutCreateWindow("ProjetoCG");

    glEnable(GL_LIGHTING);
    glEnable(GL_RESCALE_NORMAL);
    glEnable(GL_TEXTURE_2D);

    posCamara();
    
    // Funcoes de atualizacao
    glutDisplayFunc(renderScene);
    glutReshapeFunc(changeSize);
    glutIdleFunc(renderScene);

    glewInit();
    ilInit();

    if (argc == 2)
        readXml(pxml + argv[1]);
    
    float dark[4] = { 0.3, 0.3, 0.3, 1.0 };
    float white[4] = { 1.0, 1.0, 1.0, 1.0 };

    for (Light* l : lights) {
        glEnable(l->index);

        // light colors
        glLightfv(l->index, GL_AMBIENT, dark);
        glLightfv(l->index, GL_DIFFUSE, white);
        glLightfv(l->index, GL_SPECULAR, white);
        std::cout << "luzes " << l->index << "\n" << std::endl;
    }

    // Teclas precionadas
    glutSpecialFunc(keyboardFunc);

    // Definicoes do OpenGL
    glEnable(GL_DEPTH_TEST);
    glEnable(GL_CULL_FACE);

    // Ciclo
    glutMainLoop();
    
    return 1;
}