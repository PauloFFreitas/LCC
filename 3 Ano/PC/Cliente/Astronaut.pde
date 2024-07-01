class Astronaut {
  float x,y;
  int col;
  String name;
  float boost;
  // Construtor
  Astronaut(String name, float x, float y,int col,float boost) {
    this.name = name;
    this.x = x;
    this.y = y;
    this.col = col;
    this.boost = boost;
  }

  // Método para desenhar o astronauta
  void display(PApplet appc) {
    
    appc.fill(col,0,0);
    appc.ellipse(x, y, 20, 20);
  }
}
