#include <raylib.h>

int main(void)
{
    InitWindow(960, 540, "Klötzchenwelt");
    SetTargetFPS(60);

    // Die Kamera: wo sie steht, wohin sie schaut und wo oben ist
    Camera3D camera = { 0 };
    camera.position = (Vector3){ 6, 5, 8 };
    camera.target = (Vector3){ 0, 0, 0 };
    camera.up = (Vector3){ 0, 1, 0 };
    camera.fovy = 45;
    camera.projection = CAMERA_PERSPECTIVE;

    while (!WindowShouldClose())
    {
        BeginDrawing();
        ClearBackground(SKYBLUE);

        BeginMode3D(camera);
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
