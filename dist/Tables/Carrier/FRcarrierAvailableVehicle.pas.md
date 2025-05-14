<!-- tabs:start -->

#### **Documentation**

## 1. Visão Geral:

### Objetivo Principal:
O objetivo principal do código fornecido é criar uma interface de usuário para gerenciar veículos disponíveis associados a transportadoras. Ele permite que os usuários visualizem, adicionem e excluam veículos, além de realizar buscas específicas por tipo de veículo. O código implementa uma grade (grid) para exibir os dados e fornece ações para manipulação dos registros.

### Tecnologias Utilizadas:
- **Delphi**: Linguagem de programação utilizada para criar a aplicação.
- **Componentes cxGrid**: Utilizados para exibir e manipular dados em formato de tabela.
- **SOAP**: Utilizado para comunicação com serviços externos.
- **Bibliotecas Personalizadas**: Incluem `kneFRGridEditSOA`, `kneFRGridManager`, entre outras, para funcionalidades específicas.

### Tipo de Interface:
- **Grade (Grid Display)**:
  - **Colunas da Grade**:
    - `vehicleType` (Tipo de Veículo): String.
    - `description` (Descrição): String.
    - `numVehicle` (Número do Veículo): String.
    - `box` (Caixa): Boolean (Checkbox).
  - **Ações da Grade**:
    - Adicionar (`ADD`): Permite adicionar um novo veículo.
    - Excluir (`DELETE`): Permite excluir um veículo existente.

---

## 2. Descrição da Funcionalidade:

### Ações Disponíveis:
- Adicionar um novo veículo.
- Excluir um veículo existente.
- Buscar veículos por tipo ou código.

### Componentes Principais:
- **Grade (cxGrid)**: Exibe os dados dos veículos.
- **Botões de Ação**: Permitem adicionar e excluir registros.
- **Campos Personalizados**:
  - `cxEDTfindVehicleType`: Botão para buscar tipos de veículos.
  - `cxEDTRCheckBox`: Checkbox para indicar se o veículo possui caixa.

### Pseudo-código:
- Evento `OnEditValueChanged`:
  ```pseudo
  se o valor de um campo na grade for alterado, então execute a função de validação.
  ```
- Evento `OnButtonClick` do botão de busca:
  ```pseudo
  se o botão de busca for clicado, então execute a função de busca de tipo de veículo.
  ```
- Ação `ADD`:
  ```pseudo
  se o botão "Adicionar" for clicado, então abra o formulário para adicionar um novo veículo.
  ```
- Ação `DELETE`:
  ```pseudo
  se o botão "Excluir" for clicado, então remova o veículo selecionado.
  ```

---

## 3. Lógica Operacional:

### Fluxo de Execução:
1. Inicialização:
   - A interface é carregada com os componentes da grade e os campos configurados.
   - Os campos `vehicleType` e `box` são configurados com editores personalizados.
2. Interações do Usuário:
   - O usuário pode clicar nos botões de ação para adicionar ou excluir veículos.
   - O usuário pode alterar valores diretamente na grade.
   - O botão de busca permite localizar tipos de veículos.

### Dados Necessários:
- Tipo de Veículo (`vehicleType`).
- Descrição (`description`).
- Número do Veículo (`numVehicle`).
- Indicação de Caixa (`box`).

---

## 4. Regras de Negócio:

### Ações e Pré-condições:
- **Adicionar**:
  - Pré-condição: Nenhuma.
  - Ação: Abre um formulário para adicionar um novo veículo.
- **Excluir**:
  - Pré-condição: Um veículo deve estar selecionado na grade.
  - Ação: Remove o veículo selecionado.

### Filtros Disponíveis:
- Busca por tipo de veículo.
- Busca por código de veículo.

### Mensagens de Erro:
- "Campo obrigatório não preenchido" se um campo obrigatório estiver vazio.
- "Tipo de veículo inválido" se o tipo de veículo não for encontrado.

### Valores Padrão dos Campos:
- Não definidos explicitamente no código.

### Validações e Condições:
- `vehicleType`: Deve ser validado para garantir que o tipo de veículo existe.
- `box`: Aceita apenas valores `Y` (Sim) ou `N` (Não).

---

## 5. Funções Principais:

- **`Create`**:
  - Configura a grade e define os campos e ações disponíveis.
- **`m_FindVehicleType`**:
  - Executa a busca de tipos de veículos.
- **`m_FindByCodeVehicleType`**:
  - Valida e busca informações adicionais com base no código do veículo.

---

## 6. Consumo de Serviços API:

- **Serviço**: `VehicleServiceUtils`.
- **Endpoint**: Não especificado no código.
- **Dados Enviados**: Código do veículo ou tipo de veículo.
- **Dados Recebidos**: Informações detalhadas sobre o veículo.
- **Propósito**: Buscar informações de veículos.
- **Tratamento de Erros**: Não especificado no código.

---

## 7. Campos Condicionais (Lógica do Formulário):

- Não há campos condicionais explícitos no código.

---

## 8. Dependências:

### Bibliotecas Externas:
- **cxGrid**: Para exibição de dados em formato de tabela.
- **SOAPHTTPClient**: Para comunicação com serviços SOAP.

### Componentes Personalizados:
- **kneFRGridEditSOA**: Base para a interface de edição.
- **kneFRGridManager**: Gerenciamento da grade.

---

## 9. Listagem de Campos e Validações:

- `vehicleType` (tipo: string, obrigatório, não possui validação explícita no código).
- `description` (tipo: string, obrigatório, não possui validação explícita no código).
- `numVehicle` (tipo: string, obrigatório, não possui validação explícita no código).
- `box` (tipo: boolean, obrigatório, valores permitidos: `Y` ou `N`).

---

## 10. Exemplos e Diagramas:

### Diagrama de Fluxo:
Não aplicável.

### Diagrama de Sequência:
Não aplicável.

### Código HTML Representando a Grade:
```html
<table style="width: 100%; border: 1px solid black; border-collapse: collapse;">
  <thead>
    <tr>
      <th style="border: 1px solid black;">Tipo de Veículo</th>
      <th style="border: 1px solid black;">Descrição</th>
      <th style="border: 1px solid black;">Número do Veículo</th>
      <th style="border: 1px solid black;">Caixa</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td style="border: 1px solid black;">Caminhão</td>
      <td style="border: 1px solid black;">Veículo de carga</td>
      <td style="border: 1px solid black;">12345</td>
      <td style="border: 1px solid black;">Sim</td>
    </tr>
  </tbody>
</table>
```

---

## 11. Comentários Importantes no Código:

- Configuração da grade e campos personalizados no método `Create`.
- Atribuição de eventos para busca de tipos de veículos.

---

## 12. Conclusão:

O código implementa uma interface funcional para gerenciar veículos disponíveis, com suporte a ações básicas como adicionar, excluir e buscar registros. No entanto, faltam detalhes sobre validações e tratamento de erros, o que pode ser uma limitação.

---

## 13. Resumo Curto:

Interface para gerenciar veículos disponíveis, com grade para exibição de dados, suporte a ações de adicionar e excluir, e busca por tipo de veículo. Utiliza componentes cxGrid e serviços SOAP para comunicação.#### **FRcarrierAvailableVehicle.pas**

```
unit FRcarrierAvailableVehicle;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFRGridEditSOA, cxStyles, cxCustomData, cxGraphics, cxFilter,
  cxData, cxDataStorage, cxEdit, DB, cxDBData, InvokeRegistry,
  cxEditRepositoryItems, ImgList, ActnList, ExtCtrls, Rio, SOAPHTTPClient,
  DBClient, StdCtrls, Buttons, cxGridLevel, cxGridCustomTableView,
  cxGridTableView, cxGridDBTableView, cxClasses, cxControls,
  cxGridCustomView, cxGrid, sFrameAdapter, kneFRGridManager, sBitBtn,
  sPanel,kneFGFindUtils;

type
  TFRAMEcarrierAvailableVehicle = class(TFRAMEBaseGridEditSOA)
    cxEDTfindVehicleType: TcxEditRepositoryButtonItem;
    cxEDTRCheckBox: TcxEditRepositoryCheckBoxItem;
    procedure cxDBVtableEditValueChanged(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
    procedure ACTaddExecute(Sender: TObject);
  private
    { Private declarations }
    procedure m_FindVehicleType(Sender: TObject; AButtonIndex: Integer);
    procedure m_FindByCodeVehicleType(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent);  override;
  end;

var
  FRAMEcarrierAvailableVehicle: TFRAMEcarrierAvailableVehicle;

implementation

uses
  kneFindDialogSOA, kneUtils, kneInterfaces, BaseServiceUtils, Global,
  VehicleServiceUtils, kneTypes, kneFindDialog, kneDialogFactory,
  kneFREditSOA;

{$R *.dfm}

constructor TFRAMEcarrierAvailableVehicle.Create(AOwner: TComponent);
begin
  inherited;

  // SET DAS PROPRIEDADES DA FRAME
  MasterKeyFields := 'code=carrierCode';
  DataPacketName := 'Vehicle';         // o nome do detail no datapacket(metadata) � sempre no singular
  PropertyName := 'vehicles';          // nome do campo da metadata que vai conter os details
  FrameType := frtDetail;
  // configurar visibilidade de painel de ac��es e ac��es dispon�veis
  // pode ser usado em qualquer sitio do c�digo e a frame � imediatamente
  // redesenhada
  ShowActionPanel := True;
  AvailableActions := 'ADD;DELETE';

  //Definir aqui os settings da dxGrid
  with GridSettings do
  begin
    // Campos Read-Only ........................................................
    // Campos Hidden ...........................................................
    HiddenFields.Add('carrierCode');
    // Ordem Campos ............................................................
    OrderFields.Add('vehicleType');
    OrderFields.Add('description');
    OrderFields.Add('numVehicle');
    OrderFields.Add('box');
    // Key Fields ..............................................................
    KeyFields:= 'carrierCode;vehicleType';
    // Custom Editors ..........................................................
    AddCustomField('vehicleType','cxEDTfindVehicleType');
    AddCustomField('box','cxEDTRCheckBox');
  end; //with

  // Atribui��o dos eventos dos Finds  
  cxEDTfindVehicleType.Properties.OnButtonClick := m_FindVehicleType;
end;

procedure TFRAMEcarrierAvailableVehicle.m_FindByCodeVehicleType(
  Sender: TcxCustomGridTableView; AItem: TcxCustomGridTableItem);
var
  lv_Service: TVehicleServiceUtils;
  lv_TargetDescFields, lv_DescFields : TStringList;
  lv_Column: TcxGridDBColumn;
  lv_Key: string;
  lv_Cursor: TCursor;
  lv_Result: Boolean;
begin
  if Sender.Controller.EditingController.Edit.EditValue = '' then
    Exit;
  lv_Service := nil;
  lv_TargetDescFields := nil;
  lv_DescFields := nil;
  try
    lv_Column := cxDBVtable.GetColumnByFieldName('vehicleType');
    if (lv_Column <> nil) then
    begin
       lv_Key :=
```

#### **FRcarrierAvailableVehicle.dfm**

```
inherited FRAMEcarrierAvailableVehicle: TFRAMEcarrierAvailableVehicle
  inherited cxDBG: TcxGrid
    inherited cxDBVtable: TcxGridDBTableView
      OnEditValueChanged = cxDBVtableEditValueChanged
    end
  end
  inherited cxEDTR: TcxEditRepository
    object cxEDTfindVehicleType: TcxEditRepositoryButtonItem
      Properties.Buttons = <
        item
          Default = True
          Glyph.Data = {
            36030000424D3603000000000000360000002800000010000000100000000100
            18000000000000030000120B0000120B00000000000000000000FF00FF4A667C
            BE9596FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
            FFFF00FFFF00FFFF00FF6B9CC31E89E84B7AA3C89693FF00FFFF00FFFF00FFFF
            00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF4BB4FE51B5FF
            2089E94B7AA2C69592FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
            FFFF00FFFF00FFFF00FFFF00FF51B7FE51B3FF1D87E64E7AA0CA9792FF00FFFF
            00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
            51B7FE4EB2FF1F89E64E7BA2B99497FF00FFFF00FFFF00FFFF00FFFF00FFFF00
            FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF52B8FE4BB1FF2787D95F6A76FF
            00FFB0857FC09F94C09F96BC988EFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
            FF00FFFF00FF55BDFFB5D6EDBF9D92BB9B8CE7DAC2FFFFE3FFFFE5FDFADAD8C3
            B3B58D85FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFCEA795FD
            EEBEFFFFD8FFFFDAFFFFDBFFFFE6FFFFFBEADDDCAE837FFF00FFFF00FFFF00FF
            FF00FFFF00FFFF00FFFF00FFC1A091FBDCA8FEF7D0FFFFDBFFFFE3FFFFF8FFFF
            FDFFFFFDC6A99CFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFC1A091FEE3ACF1
            C491FCF2CAFFFFDDFFFFE4FFFFF7FFFFF7FFFFE9EEE5CBB9948CFF00FFFF00FF
            FF00FFFF00FFFF00FFC2A191FFE6AEEEB581F7DCAEFEFDD8FFFFDFFFFFE3FFFF
            E4FFFFE0F3ECD2BB968EFF00FFFF00FFFF00FFFF00FFFF00FFBC978CFBE7B7F4
            C791F2C994F8E5B9FEFCD8FFFFDDFFFFDCFFFFE0E2D2BAB68E86FF00FFFF00FF
            FF00FFFF00FFFF00FFFF00FFD9C3A9FFFEE5F7DCB8F2C994F5D4A5FAE8BDFDF4
            C9FDFBD6B69089FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFB58D85E8
            DEDDFFFEF2F9D8A3F4C48CF9D49FFDEAB8D0B49FB89086FF00FFFF00FFFF00FF
            FF00FFFF00FFFF00FFFF00FFFF00FFAD827FC9AA9EEFE0B7EFDFB2E7CEACB890
            86B89086FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
            00FFFF00FFBA968ABB988CB79188FF00FFFF00FFFF00FFFF00FF}
          Kind = bkGlyph
        end>
      Properties.CharCase = ecUpperCase
      Properties.ClickKey = 114
    end
    object cxEDTRCheckBox: TcxEditRepositoryCheckBoxItem
      Properties.NullStyle = nssUnchecked
      Properties.ValueChecked = 'Y'
      Properties.ValueUnchecked = 'N'
    end
  end
end
```
<!-- tabs:end -->

