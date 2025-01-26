#include "WindowsTools.h"
#include "ULA.h"
#include <windows.h>
#include <vector>
#include <map>
// Global variable for the window class name
const char g_szClassName[] = "MyWindowClass";
#define IDT_TIMER1 1  // Timer ID
constexpr int SPECY_SCREEN_WIDTH = 256;
constexpr int SPECY_SCREEN_HEIGHT = 192;
constexpr int HOST_SCREEN_WIDTH = 800;
constexpr int HOST_SCREEN_HEIGHT = 600;
constexpr int WINDOW_WIDTH = 1024;
constexpr int WINDOW_HEIGHT = 768;

unsigned int borderColor = 0;

std::map<unsigned char, std::vector<unsigned char>> keyboardMapVK = {

    {VK_SHIFT, {0xFE, 0} },
    {'Z', {0xFE, 1} },
    {'X', {0xFE, 2} },
    {'C', {0xFE, 3} },
    {'V', {0xFE, 4} },

    {'A', {0xFD, 0} },
    {'S', {0xFD, 1} },
    {'D', {0xFD, 2} },
    {'F', {0xFD, 3} },
    {'G', {0xFD, 4} },
    
    {'Q', {0xFB, 0} },
    {'W', {0xFB, 1} },
    {'E', {0xFB, 2} },
    {'R', {0xFB, 3} },
    {'T', {0xFB, 4} },

    {'1', {0xF7, 0} },
    {'2', {0xF7, 1} },
    {'3', {0xF7, 2} },
    {'4', {0xF7, 3} },
    {'5', {0xF7, 4} },

    {'0', {0xEF, 0} },
    {'9', {0xEF, 1} },
    {'8', {0xEF, 2} },
    {'7', {0xEF, 3} },
    {'6', {0xEF, 4} },

    {'P', {0xDF, 0} },
    {'O', {0xDF, 1} },
    {'I', {0xDF, 2} },
    {'U', {0xDF, 3} },
    {'Y', {0xDF, 4} },

    {VK_RETURN, {0xBF, 0} },
    {'L', {0xBF, 1} },
    {'K', {0xBF, 2} },
    {'J', {0xBF, 3} },
    {'H', {0xBF, 4} },

    {VK_SPACE, {0x7F, 0} },
    {VK_CONTROL, {0x7F, 1} },
    {'M', {0x7F, 2} },
    {'N', {0x7F, 3} },
    {'B', {0x7F, 4} }

};

std::vector<unsigned char>* getKeyBoardMap(WPARAM wParam) {

    try {

        return &keyboardMapVK.at((unsigned char)wParam);
    }
    catch (...) {

    }
    return nullptr;
}

// Window Procedure function
LRESULT CALLBACK WndProc(HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam)
{
    static ULA_PARAMS* cwp;    
    static std::vector<unsigned long> color_array;

    switch (msg)
    {
    case WM_CREATE:
        // Set a timer with a 1-second (1000 ms) interval
    {
        CREATESTRUCT* pCreate = (CREATESTRUCT*)lParam;
        cwp = (ULA_PARAMS*)pCreate->lpCreateParams;
        RECT rcClient;
        GetClientRect(hwnd, &rcClient);      
        color_array.resize(SPECY_SCREEN_WIDTH * SPECY_SCREEN_HEIGHT);
        SetTimer(hwnd, IDT_TIMER1, cwp->videoRefreshRate, NULL);
    }
        break;
    case WM_CLOSE:
        KillTimer(hwnd, IDT_TIMER1);
        DestroyWindow(hwnd);
        break;
    case WM_TIMER:
        if (wParam == IDT_TIMER1)
        { 
            // Code to execute when the timer elapses
            cwp->onVideoSignal(cwp->rom, [&](int x, int y, unsigned long color) {
            
                color_array[x + (y * SPECY_SCREEN_WIDTH)] = color;
                }, [&]() {
                    InvalidateRect(hwnd, NULL, FALSE);
            });
        }
        break;
    case WM_PAINT: {
        PAINTSTRUCT ps;        
        RECT rcClient;
        GetClientRect(hwnd, &rcClient);
        
        // Define a BITMAPINFO structure
        BITMAPINFO bmi = {};
        bmi.bmiHeader.biSize = sizeof(BITMAPINFOHEADER);
        bmi.bmiHeader.biWidth = SPECY_SCREEN_WIDTH;
        bmi.bmiHeader.biHeight = -SPECY_SCREEN_HEIGHT;  // Negative height for a top-down DIB
        bmi.bmiHeader.biPlanes = 1;
        bmi.bmiHeader.biBitCount = 32;  // 32 bits per pixel
        bmi.bmiHeader.biCompression = BI_RGB;

        // Paint the pixel data onto the window
        int client_size_x = rcClient.right - rcClient.left;
        int client_size_y = rcClient.bottom - rcClient.top;
        float offX = (client_size_x - HOST_SCREEN_WIDTH) * 0.5f;
        float offY = (client_size_y - HOST_SCREEN_HEIGHT) * 0.5f;
        HBRUSH hBrush = CreateSolidBrush(borderColor); // Red brush
        HDC hdc = BeginPaint(hwnd, &ps);
        FillRect(hdc, &rcClient, hBrush);
        StretchDIBits(
            hdc,
            (int)offX, (int)offY, HOST_SCREEN_WIDTH, HOST_SCREEN_HEIGHT,      // Destination rectangle
            0, 0, SPECY_SCREEN_WIDTH, SPECY_SCREEN_HEIGHT,      // Source rectangle
            color_array.data(),                // Pointer to pixel data
            &bmi,                     // Bitmap information
            DIB_RGB_COLORS,           // Color format
            SRCCOPY                   // Raster operation
        );
        EndPaint(hwnd, &ps);
        DeleteObject(hBrush);
        cwp->onVideoVSync(cwp->rom);
    }
        break;
    case WM_KEYDOWN: {

        cwp->onKeyPressed(getKeyBoardMap(wParam), 1);
    }
        break;
    case WM_KEYUP: {

        cwp->onKeyPressed(getKeyBoardMap(wParam), 0);
    }
        break;
    case WM_DESTROY:
        PostQuitMessage(0);
        break;
    default:
        return DefWindowProc(hwnd, msg, wParam, lParam);
    }
    return 0;
}

static HWND sHwnd;
int window_main_loop(ULA_PARAMS* params)
{
    HINSTANCE hInstance = (HINSTANCE)GetModuleHandle(nullptr);

    // Step 1: Registering the Window Class
    WNDCLASSEX wc;
    MSG msg;

    wc.cbSize = sizeof(WNDCLASSEX);
    wc.style = 0;
    wc.lpfnWndProc = WndProc;
    wc.cbClsExtra = 0;
    wc.cbWndExtra = 0;
    wc.hInstance = hInstance;
    wc.hIcon = LoadIcon(NULL, IDI_APPLICATION);
    wc.hCursor = LoadCursor(NULL, IDC_ARROW);
    wc.hbrBackground = (HBRUSH)(COLOR_WINDOW + 1);
    wc.lpszMenuName = NULL;
    wc.lpszClassName = g_szClassName;
    wc.hIconSm = LoadIcon(NULL, IDI_APPLICATION);

    if (!RegisterClassEx(&wc))
    {
        MessageBox(NULL, "Window Registration Failed!", "Error", MB_ICONEXCLAMATION | MB_OK);
        return 0;
    }

    // Step 2: Creating the Window
    sHwnd = CreateWindowEx(
        WS_EX_CLIENTEDGE,         // Optional extended window style
        g_szClassName,            // Registered window class name
        "My Window",              // Window title
        WS_OVERLAPPEDWINDOW,      // Default window style
        CW_USEDEFAULT, CW_USEDEFAULT,  // X and Y positions
        WINDOW_WIDTH, WINDOW_HEIGHT,                 // Width and Height of the window
        NULL,                     // No parent window
        NULL,                     // No menu
        hInstance,                // Application instance handle
        params                // No additional data
    );

    if (sHwnd == NULL)
    {
        MessageBox(NULL, "Window Creation Failed!", "Error", MB_ICONEXCLAMATION | MB_OK);
        return 0;
    }

    ShowWindow(sHwnd, SW_SHOW);
    UpdateWindow(sHwnd);

    // Step 3: The Message Loop
    while (GetMessage(&msg, NULL, 0, 0) > 0)
    {        
        TranslateMessage(&msg);
        DispatchMessage(&msg);
    }

    return (int)msg.wParam;
}

void window_set_border(unsigned int border) {

    borderColor = border;
}