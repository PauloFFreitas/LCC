class Planet {
  float x;
  float y;
  float radius; // raio do planeta
  int col;

  // Construtor
  Planet(float x, float y, float radius, int col) {
    this.x = x;
    this.y = y;
    this.radius = radius;
    this.col = col;
  }

  // Método para desenhar o planeta
  void display(PApplet appc) {
    appc.fill(0,0,col);
    appc.ellipse(x, y, radius*3, radius*3);
  }
}

// dist = y1-y2/x1-x2
// dist < raioplaneta + raioastronauta
