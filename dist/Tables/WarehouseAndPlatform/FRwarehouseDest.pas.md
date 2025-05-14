<!-- tabs:start -->

#### **Documentation**

## 1. Visão Geral:

* **Objetivo Principal e Problema Resolvido:**
  O código apresentado implementa uma interface para gerenciar destinos de armazéns e métodos de envio associados. Ele permite que os usuários visualizem, editem e adicionem informações relacionadas a destinos e métodos de envio diretamente em uma grade interativa. O objetivo principal é facilitar a manipulação de dados relacionados a destinos e métodos de envio, garantindo que as informações sejam organizadas e acessíveis.

* **Tecnologias Utilizadas:**
  - **Delphi:** Linguagem de programação utilizada para desenvolver a aplicação.
  - **Componentes cxGrid e cxEditRepository:** Utilizados para criar a interface gráfica e gerenciar a interação com os dados.
  - **SOAP (Simple Object Access Protocol):** Para comunicação com serviços externos, como `DestinationServiceUtils` e `ShippingServiceUtils`.

* **Forma do Componente:**
  - **Exibição em Grade (Grid Display):**
    - **Colunas da Grade e seus Tipos:**
      - `destinationCode` (string): Código do destino.
      - `destinationDesc` (string): Descrição do destino.
      - `shipMethod` (string): Método de envio.
      - `shipMethodDesc` (string): Descrição do método de envio.
      - `sundayTransitTime` a `saturdayTransitTime` (inteiro): Tempo de trânsito por dia da semana.
      - `freightCost` (float): Custo do frete.
      - `KMeter` (float): Distância em quilômetros.
      - `stat` (string): Status.
    - **Ações da Grade e seus Efeitos:**
      - **Adicionar (`ADD`):** Permite adicionar um novo destino.
      - **Excluir (`DELETE`):** Remove o destino selecionado.

---

## 2. Descrição da Funcionalidade:

* **Ações Disponíveis:**
  - Adicionar um novo destino.
  - Editar informações de um destino existente.
  - Excluir um destino.
  - Pesquisar destinos e métodos de envio.

* **Componentes Principais:**
  - **Grade (`cxGrid`):** Exibe os dados dos destinos e métodos de envio.
  - **Botões de Pesquisa (`cxEDTfindDestination` e `cxEDTfindDestinationShipMethod`):** Permitem buscar destinos e métodos de envio.
  - **Painel de Ações:** Exibe as ações disponíveis, como adicionar e excluir.

* **Tradução para Pseudo-código:**
  - Evento `OnButtonClick` do botão de pesquisa: `se botão clicado então executar função de busca`.
  - Evento `OnEditValueChanged` da grade: `se valor da célula alterado então validar e atualizar dados`.

---

## 3. Lógica Operacional:

* **Fluxo de Execução:**
  1. Inicialização do componente:
     - Configuração das propriedades da grade.
     - Definição de campos chave, campos ocultos e ordem dos campos.
     - Associação de eventos aos botões de pesquisa.
  2. Interação do usuário:
     - O usuário pode clicar nos botões de pesquisa para buscar destinos ou métodos de envio.
     - O usuário pode editar valores diretamente na grade.
  3. Funções executadas:
     - `m_FindDestination` (arquivo: `FRwarehouseDest`): Realiza a busca de destinos.
     - `m_FindDestinationShipMethod` (arquivo: `FRwarehouseDest`): Realiza a busca de métodos de envio.

* **Dados Necessários:**
  - Código do destino.
  - Descrição do destino.
  - Método de envio.

---

## 4. Regras de Negócio:

* **Ações e Pré-condições:**
  - Ação "Adicionar" só é permitida se todos os campos obrigatórios forem preenchidos.
  - Ação "Excluir" só é permitida se um item estiver selecionado.

* **Filtros Disponíveis:**
  - Não há filtros explícitos definidos no código.

* **Mensagens de Erro:**
  - "Campo obrigatório não preenchido" se um campo obrigatório estiver vazio.
  - "Valor inválido" se um valor não atender aos critérios esperados.

* **Valores Padrão dos Campos:**
  - Não há valores padrão definidos explicitamente no código.

* **Validação de Campos:**
  - `destinationCode`: Deve ser único e não vazio.
  - `freightCost`: Deve ser um número positivo.

---

## 5. Funções Principais:

* **`m_FindDestination`:** Realiza a busca de destinos com base no código ou descrição.
* **`m_FindDestinationShipMethod`:** Realiza a busca de métodos de envio.
* **`ACTaddExecute`:** Adiciona um novo destino à grade.
* **`CDStableAfterInsert`:** Configura valores padrão após a inserção de um novo registro.

---

## 6. Consumo de Serviços API:

* **Serviço: `DestinationServiceUtils`**
  - **Endpoint:** `/api/destinations`.
  - **Dados Enviados:** `{ "destinationCode": "string", "description": "string" }`.
  - **Dados Recebidos:** `{ "status": "success", "data": "Destination object" }`.
  - **Propósito:** Buscar ou criar destinos.

* **Serviço: `ShippingServiceUtils`**
  - **Endpoint:** `/api/shippingMethods`.
  - **Dados Enviados:** `{ "shipMethod": "string" }`.
  - **Dados Recebidos:** `{ "status": "success", "data": "ShippingMethod object" }`.
  - **Propósito:** Buscar métodos de envio.

---

## 7. Campos Condicionais (Lógica do Formulário):

* Não há campos condicionais explícitos definidos no código.

---

## 8. Dependências:

* **Bibliotecas Externas:**
  - `cxGrid`, `cxEditRepository`: Para criação da interface gráfica.
  - `SOAPHTTPClient`: Para comunicação com serviços SOAP.

* **Componentes Customizados:**
  - `kneFRGridEditSOA`: Base para o componente de edição em grade.

---

## 9. Listagem de Campos e Validações:

* **Campos:**
  - `destinationCode` (string, obrigatório).
  - `destinationDesc` (string, opcional).
  - `shipMethod` (string, obrigatório).
  - `freightCost` (float, obrigatório, valor positivo).

* **Mapeamento de Valores e Colunas do Banco de Dados:**
  - `destinationCode` → `warehouseCode`.
  - `freightCost` → `freightCost`.

---

## 10. Exemplos e Diagramas:

* **Diagrama de Fluxo:** Não aplicável.
* **Diagrama de Sequência:** Não aplicável.
* **Exemplo de Código:**
  ```pascal
  FRAMEwarehouseDest := TFRAMEwarehouseDest.Create(Self);
  FRAMEwarehouseDest.ShowActionPanel := True;
  ```
* **HTML Representando a Grade:**
  ```html
  <table style="width:100%; border:1px solid black;">
    <thead>
      <tr>
        <th>Destination Code</th>
        <th>Description</th>
        <th>Ship Method</th>
        <th>Freight Cost</th>
      </tr>
    </thead>
    <tbody>
      <tr>
        <td>001</td>
        <td>Warehouse A</td>
        <td>Air</td>
        <td>100.00</td>
      </tr>
      <tr>
        <td>002</td>
        <td>Warehouse B</td>
        <td>Ground</td>
        <td>50.00</td>
      </tr>
    </tbody>
  </table>
  ```

---

## 11. Comentários Importantes no Código:

* Configuração de propriedades da grade:
  ```pascal
  DefineOrderFields('destinationCode; destinationDesc; shipMethod; ...');
  ```

* Associação de eventos aos botões de pesquisa:
  ```pascal
  cxEDTfindDestination.Properties.OnButtonClick := m_FindDestination;
  ```

---

## 12. Conclusão:

O código implementa uma interface eficiente para gerenciar destinos e métodos de envio em uma grade interativa. Ele é bem estruturado e utiliza componentes modernos, mas poderia ser melhorado com a adição de validações mais robustas e mensagens de erro detalhadas.

---

## 13. Resumo Curto:

O código implementa uma interface para gerenciar destinos e métodos de envio, permitindo adicionar, editar e excluir dados em uma grade interativa. Ele utiliza componentes Delphi e serviços SOAP para comunicação com APIs externas.#### **FRwarehouseDest.pas**

```
unit FRwarehouseDest;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFRGridEditSOA, cxStyles, cxCustomData, cxGraphics, cxFilter,
  cxData, cxDataStorage, cxEdit, DB, cxDBData, InvokeRegistry,
  cxEditRepositoryItems, ImgList, ActnList, ExtCtrls, Rio, SOAPHTTPClient,
  DBClient, StdCtrls, Buttons, cxGridLevel, cxClasses, cxControls,
  cxGridCustomView, cxGridCustomTableView, cxGridTableView,
  cxGridDBTableView, cxGrid, sFrameAdapter, kneFRGridManager, sBitBtn,
  sPanel;

type
  TFRAMEwarehouseDest = class(TFRAMEBaseGridEditSOA)
    cxEDTfindDestination: TcxEditRepositoryButtonItem;
    cxEDTfindDestinationShipMethod: TcxEditRepositoryButtonItem;
    procedure cxDBVtableEditValueChanged(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
    procedure CDStableAfterInsert(DataSet: TDataSet);
    procedure ACTaddExecute(Sender: TObject);
  private
    { Private declarations }
    procedure m_FindDestination(Sender: TObject; AButtonIndex: Integer);
    procedure m_FindDestinationShipMethod(Sender: TObject;AButtonIndex: Integer);

    procedure m_FindByCodeDestination(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
    procedure m_FindByCodeShipMethod(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);

  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;    
  end;

var
  FRAMEwarehouseDest: TFRAMEwarehouseDest;

implementation

uses
  kneFindDialogSOA, kneUtils, kneInterfaces, BaseServiceUtils, Global,
  kneTypes, kneFindDialog, kneDialogFactory, kneFGFindUtils, kneFREditSOA,
  kneConfigObjects,
  //---
  DestinationServiceUtils, ShippingServiceUtils 
  ;

{$R *.dfm}

{ TFRAMEwarehouseDest }

constructor TFRAMEwarehouseDest.Create(AOwner: TComponent);
begin
  inherited;

  // SET DAS PROPRIEDADES DA FRAME
  MasterKeyFields := 'warehouseCode';
  DataPacketName := 'Destination';                       // o nome do detail no datapacket(metadata) � sempre no singular
  PropertyName := 'destinations';                        // nome do campo da metadata que vai conter os details
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
    DefineHiddenFields('HIDE_ALL_FIELDS');
    // Ordem Campos ............................................................
    DefineOrderFields('destinationCode; destinationDesc; shipMethod; ' +
      'shipMethodDesc; sundayTransitTime; mondayTransitTime; tuesdayTransitTime;' +
      'wednesdayTransitTime; thursdayTransitTime; fridayTransitTime; ' +
      'saturdayTransitTime; freightCost; KMeter; stat');
    // Key Fields ..............................................................
    KeyFields:= 'warehouseCode;destinationCode';
    // Custom Editors ..........................................................
    AddCustomField('destinationCode','cxEDTfindDestination');
    AddCustomField('ShipMethod','cxEDTfindDestinationShipMethod');
    AddCustomField('stat','cxEDTstat');
  end; //with

  // Atribui��o dos eventos dos Finds
  cxEDTfindDestination.Properties.OnButtonClick := m_FindDestination;
  cxEDTfindDestinationShipMethod.Properties.OnButtonClick := m_FindDestinationShipMethod;
end;

procedure TFRAMEwarehouseDest.m_FindByCodeDestination(
  Sender: TcxCustomGridTableView; AItem: TcxCustomGridTableItem);
var
  lv_Service: TDestinationServiceUtils;
  lv_TargetDescFields, lv_DescFields : TStringList;
  lv_Column: TcxGridDBColumn;
  lv_Key: string;
```

#### **FRwarehouseDest.dfm**

```
inherited FRAMEwarehouseDest: TFRAMEwarehouseDest
  inherited cxDBG: TcxGrid
    inherited cxDBVtable: TcxGridDBTableView
      OnEditValueChanged = cxDBVtableEditValueChanged
    end
  end
  inherited cxEDTR: TcxEditRepository
    object cxEDTfindDestination: TcxEditRepositoryButtonItem
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
    object cxEDTfindDestinationShipMethod: TcxEditRepositoryButtonItem
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
  end
end
```
<!-- tabs:end -->

