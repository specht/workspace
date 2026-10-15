#include <raylib.h>

int main(void)
{
    InitWindow(960, 540, "Klötzchenwelt");
    SetTargetFPS(60);

    // Die Kamera: wo sie steht, wohin sie schaut und wo oben ist
    Camera3D kamera = { 0 };
    kamera.position = (Vector3){ 6, 5, 8 };
    kamera.target = (Vector3){ 0, 0, 0 };
    kamera.up = (Vector3){ 0, 1, 0 };
    kamera.fovy = 45;
    kamera.projection = CAMERA_PERSPECTIVE;

    while (!WindowShouldClose())
    {
        BeginDrawing();
        ClearBackground(SKYBLUE);

        BeginMode3D(kamera);
        DrawGrid(10, 1);
        DrawCube((Vector3){ 0, 0.5f, 0 }, 1, 1, 1, ORANGE);
        DrawCubeWires((Vector3){ 0, 0.5f, 0 }, 1, 1, 1, MAROON);
        EndMode3D();

        DrawText("Hallo, 3D!", 20, 20, 30, DARKBLUE);
        EndDrawing();
    }

    CloseWindow();
    return 0;
}
