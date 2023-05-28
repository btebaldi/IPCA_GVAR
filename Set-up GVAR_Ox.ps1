
$mat_files = ""
while (-not (Test-Path -Path $mat_files)) {
    $mat_files = Read-Host -Prompt 'Input main dir of Ox GVAR-IIS matrices'
    if ([string]::IsNullOrEmpty($mat_files)) {
        $mat_files = "C:/Users/Teo/Documents/GitHub/IPCA_GVAR/Ox/mat_files/"
    }
}

Write-Host "using the path: $mat_files"

# CREATE txMatPathRawMatrix
New-Item -Path $mat_files -Name "RawMatrix" -ItemType "directory" -Force 

# CREATE A_Matrix
New-Item -Path $mat_files -Name "A_Matrix" -ItemType "directory" -Force

# CREATE W_mat
New-Item -Path $mat_files -Name "W_mat" -ItemType "directory" -Force

# strMatrixDePesos = "PesoIpca.mat";

# CREATE G_Matrix
New-Item -Path $mat_files -Name "G_Matrix" -ItemType "directory" -Force

# CREATE Result_Matrix
New-Item -Path $mat_files -Name "Result_Matrix" -ItemType "directory" -Force

# CREATE Cointegration
New-Item -Path $mat_files -Name "Cointegration" -ItemType "directory" -Force

# CREATE database
# New-Item -Path $mat_files -Name "database" -ItemType "directory"
# txDbase = "./database/IPCA-baseOx_v1.in7";
# txDbaseMacroVariables = "./database/IPCA-baseOx_v1.in7";

