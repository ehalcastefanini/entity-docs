<!-- tabs:start -->

#### **Documentation**

## 1. Visão Geral:

* **Objetivo Principal e Problema Resolvido:**
  O código apresentado implementa um componente de interface gráfica para gerenciar tipos de veículos associados a consignatários. Ele permite a exibição, edição, adição e exclusão de registros de tipos de veículos em um grid interativo. O objetivo principal é facilitar a manipulação de dados relacionados a tipos de veículos de forma eficiente e organizada.

* **Tecnologias Utilizadas:**
  - Delphi (VCL e componentes visuais como `TcxGrid`, `TcxEditRepository`, etc.).
  - SOAP para comunicação com serviços externos.
  - Manipulação de datasets com `TClientDataSet`.

* **Forma do Componente:**
  - **Grid Display:**
    - **Colunas do Grid e seus Tipos:**
      - `vehTypeCode` (Código do Tipo de Veículo) - String.
      - `vehType` (Descrição do Tipo de Veículo) - String.
      - `defaultVehicle` (Veículo Padrão) - Checkbox (Sim/Não).
    - **Ações do Grid e seus Efeitos:**
      - Adicionar (`ADD`): Permite adicionar um novo tipo de veículo.
      - Excluir (`DELETE`): Remove o tipo de veículo selecionado.

## 2. Descrição da Funcionalidade:

* **Ações Específicas:**
  - Adicionar um novo tipo de veículo.
  - Editar os valores de um tipo de veículo existente.
  - Excluir um tipo de veículo selecionado.
  - Pesquisar tipos de veículos por código ou descrição.

* **Componentes Principais:**
  - `TcxGrid`: Exibe os dados em formato de tabela.
  - `TcxEditRepositoryCheckBoxItem`: Permite marcar/desmarcar o campo "Veículo Padrão".
  - `TClientDataSet`: Gerencia os dados exibidos no grid.

* **Tradução para Pseudo-código:**
  - Evento `OnEditValueChanged`:
    ```pseudo
    se valor do campo no grid for alterado então
        executar lógica de validação ou atualização
    ```
  - Evento `OnNewRecord`:
    ```pseudo
    ao criar um novo registro então
        inicializar valores padrão
    ```
  - Ação `ACTaddExecute`:
    ```pseudo
    se botão "Adicionar" for clicado então
        criar novo registro no dataset
    ```

## 3. Lógica Operacional:

* **Fluxo de Execução:**
  - Inicialização:
    - O componente é carregado e configurado no construtor `Create`.
    - Configurações do grid, como campos ocultos, ordem de exibição e editores personalizados, são definidas.
  - Interações do Usuário:
    - O usuário pode adicionar, editar ou excluir registros no grid.
    - Eventos como `OnEditValueChanged` e `OnNewRecord` são disparados conforme necessário.

* **Dados Necessários:**
  - Código do Tipo de Veículo (`vehTypeCode`).
  - Descrição do Tipo de Veículo (`vehType`).
  - Indicação se é o veículo padrão (`defaultVehicle`).

## 4. Regras de Negócio:

* **Ações e Pré-condições:**
  - **Adicionar:** Disponível sempre.
  - **Excluir:** Disponível apenas se um registro estiver selecionado.

* **Filtros Disponíveis:**
  - Não há filtros explícitos definidos no código.

* **Mensagens de Erro:**
  - Não há mensagens de erro explícitas definidas no código.

* **Valores Padrão dos Campos:**
  - `defaultVehicle`: Valor padrão "N" (Não).

* **Validações e Condições dos Campos:**
  - `vehTypeCode`: Deve ser preenchido.
  - `defaultVehicle`: Aceita apenas "Y" (Sim) ou "N" (Não).

## 5. Funções Principais:

* **Descrição das Funções:**
  - `Create`: Configura o grid e define propriedades iniciais.
  - `m_FindVehType`: Lida com a pesquisa de tipos de veículos.
  - `m_FindByCodeVehicleType`: Pesquisa tipos de veículos por código.

## 6. Consumo de Serviços API:

* **Chamadas a Serviços Externos:**
  - Serviço: `VehicleServiceUtils`.
  - Finalidade: Buscar informações sobre tipos de veículos.
  - Dados Enviados: Não especificado no código.
  - Dados Recebidos: Não especificado no código.
  - Tratamento de Erros: Não especificado no código.

## 7. Campos Condicionais (Lógica do Formulário):

* Não há campos condicionais definidos no código.

## 8. Dependências:

* **Bibliotecas Externas:**
  - `SOAPHTTPClient`: Para comunicação com serviços SOAP.
  - `cxGrid`, `cxEditRepository`: Componentes visuais para exibição e edição de dados.

* **Componentes Customizados:**
  - `TFRAMEBaseGridEditSOA`: Classe base para o frame.

## 9. Listagem de Campos e Validações:

* **Campos:**
  - `vehTypeCode` (tipo: string, obrigatório).
  - `vehType` (tipo: string, obrigatório).
  - `defaultVehicle` (tipo: checkbox, valor padrão: "N").

* **Mapeamento de Valores e Colunas do Banco de Dados:**
  - `vehTypeCode` → Coluna `vehTypeCode`.
  - `vehType` → Coluna `vehType`.
  - `defaultVehicle` → Coluna `defaultVehicle`.

## 10. Exemplos e Diagramas:

* **Fluxograma:** Não aplicável.
* **Diagrama de Sequência:** Não aplicável.
* **Exemplo de Código:**
  ```delphi
  FRAMEconsVehType := TFRAMEconsVehType.Create(Self);
  FRAMEconsVehType.Parent := Self;
  ```
* **HTML Representando o Grid:**
  ```html
  <table style="border: 1px solid black; width: 100%;">
    <thead>
      <tr>
        <th>vehTypeCode</th>
        <th>vehType</th>
        <th>defaultVehicle</th>
      </tr>
    </thead>
    <tbody>
      <tr>
        <td>001</td>
        <td>Carro</td>
        <td>Sim</td>
      </tr>
      <tr>
        <td>002</td>
        <td>Caminhão</td>
        <td>Não</td>
      </tr>
    </tbody>
  </table>
  ```

## 11. Comentários Importantes no Código:

* Configuração inicial do grid no método `Create`.
* Definição de campos ocultos e ordem de exibição no grid.

## 12. Conclusão:

O código implementa um componente eficiente para gerenciar tipos de veículos associados a consignatários. Ele é bem estruturado e utiliza boas práticas de configuração de grids e datasets. No entanto, faltam mensagens de erro e validações mais robustas.

## 13. Resumo Curto:

O código implementa um grid interativo para gerenciar tipos de veículos, permitindo adicionar, editar e excluir registros. Ele utiliza componentes visuais avançados e comunicação SOAP para integração com serviços externos.#### **FRconsVehType.pas**

```
unit FRconsVehType;

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
  TFRAMEconsVehType = class(TFRAMEBaseGridEditSOA)
    cxEDTdefaultVehicle: TcxEditRepositoryCheckBoxItem;
    procedure cxDBVtableEditValueChanged(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
    procedure CDStableNewRecord(DataSet: TDataSet);
    procedure ACTaddExecute(Sender: TObject);
  private
    { Private declarations }
    procedure m_FindVehType(Sender: TObject; AButtonIndex: Integer);
    procedure m_FindByCodeVehicleType(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
  end;

var
  FRAMEconsVehType: TFRAMEconsVehType;

implementation

uses
  kneInterfaces, kneUtils, kneFindDialogSOA, BaseServiceUtils, Global,
  VehicleServiceUtils, kneTypes,kneFindDialog,kneDialogFactory,
  kneConfigObjects;

{$R *.dfm}

{ TFRAMEconsVehType }

constructor TFRAMEconsVehType.Create(AOwner: TComponent);
begin
  inherited;
  // SET DAS PROPRIEDADES DA FRAME
  MasterKeyFields := 'consCode';
  DataPacketName := 'ConsigneeVehType';
  PropertyName := 'consigneeVehTypes';
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
    HiddenFields.Add('consCode');
    HiddenFields.Add('updBy');
    HiddenFields.Add('lastUpd');
    // Ordem Campos ............................................................
    OrderFields.Add('vehTypeCode');
    OrderFields.Add('vehType');
    OrderFields.Add('defaultVehicle');
    // Key Fields ..............................................................
    KeyFields:= 'consCode;vehTypeCode';
    // Custom Editors ..........................................................
    AddCustomField('vehTypeCode','cxEDTfind');
    AddCustomField('defaultVehicle','cxEDTdefaultVehicle');
  end; //with

  // Atribui��o dos eventos dos Finds
  cxEDTfind.Properties.OnButtonClick := m_FindVehType;
  //@@@@@
  CDStable.Tag := 6;
  DStable.Tag := 6;
end;

procedure TFRAMEconsVehType.m_FindByCodeVehicleType(
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
```

#### **FRconsVehType.dfm**

```
inherited FRAMEconsVehType: TFRAMEconsVehType
  inherited cxDBG: TcxGrid
    inherited cxDBVtable: TcxGridDBTableView
      OnEditValueChanged = cxDBVtableEditValueChanged
    end
  end
  inherited CDStable: TClientDataSet
    OnNewRecord = CDStableNewRecord
  end
  inherited cxEDTR: TcxEditRepository
    inherited cxEDTfind: TcxEditRepositoryButtonItem
      Properties.CharCase = ecUpperCase
    end
    object cxEDTdefaultVehicle: TcxEditRepositoryCheckBoxItem
      Properties.ValueChecked = 'Y'
      Properties.ValueUnchecked = 'N'
    end
  end
end
```
<!-- tabs:end -->

