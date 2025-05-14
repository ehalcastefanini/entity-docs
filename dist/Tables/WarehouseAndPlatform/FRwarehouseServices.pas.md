<!-- tabs:start -->

#### **Documentation**

## 1. Visão Geral:

* **Objetivo Principal e Problema Resolvido:**
  O código implementa um componente de interface gráfica para gerenciar serviços de armazém. Ele permite que os usuários visualizem, editem e interajam com dados relacionados a serviços, como tipo de serviço, transportadora, método de envio, veículo, e destinos. O objetivo principal é fornecer uma interface eficiente para manipular esses dados em um formato de grade.

* **Tecnologias Utilizadas:**
  - **Delphi:** Linguagem de programação utilizada para criar a aplicação.
  - **Componentes cxGrid e cxEditRepository:** Utilizados para criar e gerenciar a interface gráfica.
  - **SOAP (Simple Object Access Protocol):** Para comunicação com serviços externos.
  - **DBClient:** Para manipulação de dados em memória.

* **Forma do Componente:**
  - **Exibição em Grade (Grid Display):**
    - **Colunas da Grade e seus Tipos:**
      - `serviceType` (ComboBox).
      - `applyServico` (ComboBox).
      - `serviceCode` (Texto).
      - `name` (Texto).
      - `entityCode` (Texto).
      - `entityDesc` (Texto).
      - `shipMethod` (Texto).
      - `shipMethodDesc` (Texto).
      - `vehicleType` (Texto).
      - `vehicleTypeDesc` (Texto).
      - `start` (Texto).
      - `startDesc` (Texto).
      - `destination` (Texto).
      - `destinationDesc` (Texto).
      - `contract` (Texto).
    - **Ações da Grade e seus Efeitos:**
      - Adicionar (`ADD`): Adiciona um novo serviço.
      - Excluir (`DELETE`): Remove o serviço selecionado.

---

## 2. Descrição da Funcionalidade:

* **Ações Disponíveis:**
  - Adicionar um novo serviço.
  - Editar valores diretamente na grade.
  - Pesquisar serviços, transportadoras, métodos de envio, veículos e destinos.

* **Componentes Principais:**
  - **Grade (`cxGrid`):** Exibe os dados em formato tabular.
  - **Itens de Repositório (`cxEditRepository`):** Fornecem elementos interativos como botões e caixas de seleção.
  - **Painel de Ações:** Permite adicionar ou excluir serviços.

* **Tradução para Pseudo-código:**
  - Evento `OnEditValueChanged`: `se valor da célula for alterado, então execute validação ou ação associada`.
  - Botão "Adicionar": `se botão "Adicionar" for clicado, então execute a função de adicionar serviço`.
  - Botão "Pesquisar": `se botão "Pesquisar" for clicado, então abra o diálogo de pesquisa`.

---

## 3. Lógica Operacional:

* **Fluxo de Execução:**
  1. Inicialização do componente (`Create`):
     - Configurações da grade e propriedades do frame são definidas.
     - Painel de ações é exibido.
  2. Interação do Usuário:
     - Usuário pode editar valores diretamente na grade.
     - Botões de pesquisa permitem buscar dados relacionados.
  3. Funções Executadas:
     - `m_FindService`: Pesquisa serviços.
     - `m_FindServiceCarrier`: Pesquisa transportadoras.
     - `m_FindServiceDestinationD`: Pesquisa destinos finais.
     - `m_FindServiceDestinationS`: Pesquisa destinos iniciais.
     - `m_FindServiceShipMethod`: Pesquisa métodos de envio.
     - `m_FindServiceVehicle`: Pesquisa veículos.

* **Dados Necessários:**
  - Tipo de serviço, transportadora, método de envio, veículo, destinos, entre outros.

---

## 4. Regras de Negócio:

* **Ações e Pré-condições:**
  - Botão "Adicionar" só deve ser habilitado se os campos obrigatórios estiverem preenchidos.
  - Botão "Excluir" só deve ser habilitado se um item estiver selecionado.

* **Filtros Disponíveis:**
  - Não há filtros explícitos definidos no código.

* **Mensagens de Erro:**
  - "Campo obrigatório não preenchido" se um campo obrigatório estiver vazio.
  - "Valor inválido" se um valor não atender aos critérios esperados.

* **Valores Padrão dos Campos:**
  - Não há valores padrão explicitamente definidos no código.

* **Validações e Condições dos Campos:**
  - `serviceType`: Deve ser selecionado de uma lista fixa.
  - `applyServico`: Deve ser selecionado de uma lista fixa.
  - Outros campos: Não possuem validações explícitas no código.

---

## 5. Funções Principais:

* **`m_FindService`:** Pesquisa serviços com base em critérios definidos.
* **`m_FindServiceCarrier`:** Pesquisa transportadoras.
* **`m_FindServiceDestinationD`:** Pesquisa destinos finais.
* **`m_FindServiceDestinationS`:** Pesquisa destinos iniciais.
* **`m_FindServiceShipMethod`:** Pesquisa métodos de envio.
* **`m_FindServiceVehicle`:** Pesquisa veículos.

---

## 6. Consumo de Serviços API:

* Não há chamadas explícitas a serviços externos no código fornecido.

---

## 7. Campos Condicionais (Lógica do Formulário):

* Não há campos condicionais definidos no código.

---

## 8. Dependências:

* **Bibliotecas Externas:**
  - `cxGrid`, `cxEditRepository`: Para criação da interface gráfica.
  - `SOAPHTTPClient`: Para comunicação com serviços SOAP.
  - `DBClient`: Para manipulação de dados.

* **Componentes Customizados:**
  - `TFRAMEBaseGridEditSOA`: Classe base para o frame.

---

## 9. Listagem de Campos e Validações:

* **Campos:**
  - `serviceType` (ComboBox, obrigatório).
  - `applyServico` (ComboBox, obrigatório).
  - `serviceCode` (Texto, não definido no código).
  - `name` (Texto, não definido no código).
  - Outros campos: Não possuem validações explícitas no código.

* **Mapeamento de Valores e Colunas do Banco de Dados:**
  - `serviceType` → `serviceType`.
  - `applyServico` → `applyServico`.
  - Outros campos seguem o mesmo padrão.

---

## 10. Exemplos e Diagramas:

* **Fluxograma:** Não aplicável.
* **Diagrama de Sequência:** Não aplicável.
* **Exemplo de Código:**
  ```pascal
  FRAMEwarehouseServices := TFRAMEwarehouseServices.Create(Self);
  FRAMEwarehouseServices.ShowActionPanel := True;
  ```
* **HTML Representando a Grade:**
  ```html
  <table style="border: 1px solid black; width: 100%;">
    <thead>
      <tr>
        <th>Tipo de Serviço</th>
        <th>Aplicar Serviço</th>
        <th>Código do Serviço</th>
        <th>Nome</th>
        <th>Código da Entidade</th>
      </tr>
    </thead>
    <tbody>
      <tr>
        <td>PLAN</td>
        <td>Sim</td>
        <td>001</td>
        <td>Serviço A</td>
        <td>ENT001</td>
      </tr>
    </tbody>
  </table>
  ```

---

## 11. Comentários Importantes no Código:

* **Configuração do Frame:**
  ```pascal
  MasterKeyFields := 'warehouseCode';
  DataPacketName := 'Service';
  PropertyName := 'services';
  FrameType := frtDetail;
  ```

* **Configuração da Grade:**
  ```pascal
  DefineHiddenFields('HIDE_ALL_FIELDS');
  ```

---

## 12. Conclusão:

O código fornece uma interface robusta para gerenciar serviços de armazém, com suporte para edição em grade e pesquisa de dados relacionados. No entanto, faltam validações explícitas e mensagens de erro detalhadas, o que pode impactar a experiência do usuário.

---

## 13. Resumo Curto:

O código implementa um componente de interface gráfica para gerenciar serviços de armazém, permitindo edição em grade, pesquisa de dados e ações como adicionar e excluir serviços. Ele utiliza Delphi e componentes visuais para criar uma interface eficiente e interativa.#### **FRwarehouseServices.pas**

```
unit FRwarehouseServices;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFRGridEditSOA, cxStyles, cxCustomData, cxGraphics, cxFilter,
  cxData, cxDataStorage, cxEdit, DB, cxDBData, InvokeRegistry,
  cxEditRepositoryItems, ImgList, ActnList, ExtCtrls, Rio, SOAPHTTPClient,
  DBClient, StdCtrls, Buttons, cxGridLevel, cxGridCustomTableView,
  cxGridTableView, cxGridDBTableView, cxClasses, cxControls,
  cxGridCustomView, cxGrid, sFrameAdapter, kneFRGridManager, sBitBtn,
  sPanel;

type
  TFRAMEwarehouseServices = class(TFRAMEBaseGridEditSOA)
    cxEDTserviceType: TcxEditRepositoryComboBoxItem;
    cxEDTfindServiceCarrier: TcxEditRepositoryButtonItem;
    cxEDTfindService: TcxEditRepositoryButtonItem;
    cxEDTfindServiceShipMethod: TcxEditRepositoryButtonItem;
    cxEDTfindServiceVehicle: TcxEditRepositoryButtonItem;
    cxEDTfindServiceDestinationS: TcxEditRepositoryButtonItem;
    cxEDTfindServiceDestinationD: TcxEditRepositoryButtonItem;
    cxEDTRapplyServico: TcxEditRepositoryComboBoxItem;
    procedure cxDBVtableEditValueChanged(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
    procedure ACTaddExecute(Sender: TObject);
  private
    { Private declarations }
    procedure m_FindService(Sender: TObject; AButtonIndex: Integer);
    procedure m_FindServiceCarrier(Sender: TObject; AButtonIndex: Integer);
    procedure m_FindServiceDestinationD(Sender: TObject; AButtonIndex: Integer);
    procedure m_FindServiceDestinationS(Sender: TObject; AButtonIndex: Integer);
    procedure m_FindServiceShipMethod(Sender: TObject; AButtonIndex: Integer);
    procedure m_FindServiceVehicle(Sender: TObject; AButtonIndex: Integer);
    procedure m_FindByCodeService(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
    procedure m_FindByCodeCarrier(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
    procedure m_FindByCodeDestinationD(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
    procedure m_FindByCodeDestinationS(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
    procedure m_FindByCodeShipMethod(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
    procedure m_FindByCodeVehicle(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent);  override;
  end;
                                        
var
  FRAMEwarehouseServices: TFRAMEwarehouseServices;

implementation

uses
  kneFindDialogSOA, kneUtils, kneInterfaces, Global, BaseServiceUtils,
  kneTypes, kneFindDialog, kneDialogFactory, kneFGFindUtils,
  DestinationServiceUtils, ShippingServiceUtils,
  VehicleServiceUtils, ServiceServiceUtils, CarrierServiceUtils,
  FRfindCriteriaDestination, kneConfigObjects;

const
  mc_GRID_FIELDS = 'serviceType;applyServico;serviceCode;name;entityCode;' // [11-06-2015, #21987]
    + 'entityDesc;shipMethod;shipMethodDesc;vehicleType;vehicleTypeDesc;'
    + 'start;startDesc;destination;destinationDesc;contract';

{$R *.dfm}

constructor TFRAMEwarehouseServices.Create(AOwner: TComponent);
begin
  inherited;

  // SET DAS PROPRIEDADES DA FRAME
  MasterKeyFields := 'warehouseCode';
  DataPacketName := 'Service'; // o nome do detail no datapacket(metadata) � sempre no singular
  PropertyName := 'services';  // nome do campo da metadata que vai conter os details
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
//    HiddenFields.Add('warehouseCode');
//    HiddenFields.Add('updBy');
//    HiddenFields.Add('lastUpd');
    DefineHiddenFields('HIDE_ALL_FIELDS');
    // Ordem Campos ............................................................
//    OrderFields.Add('serviceType');
//    OrderFields.Add('serviceCode');
//    OrderFields.Add('name');
////    OrderFields.Add('serviceDesc');
```

#### **FRwarehouseServices.dfm**

```
inherited FRAMEwarehouseServices: TFRAMEwarehouseServices
  inherited cxDBG: TcxGrid
    inherited cxDBVtable: TcxGridDBTableView
      OnEditValueChanged = cxDBVtableEditValueChanged
    end
  end
  inherited cxEDTR: TcxEditRepository
    object cxEDTserviceType: TcxEditRepositoryComboBoxItem
      Properties.DropDownListStyle = lsEditFixedList
      Properties.Items.Strings = (
        'PLAN'
        'PERFIL')
    end
    object cxEDTfindServiceCarrier: TcxEditRepositoryButtonItem
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
    object cxEDTfindService: TcxEditRepositoryButtonItem
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
    object cxEDTfindServiceShipMethod: TcxEditRepositoryButtonItem
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
```
<!-- tabs:end -->

