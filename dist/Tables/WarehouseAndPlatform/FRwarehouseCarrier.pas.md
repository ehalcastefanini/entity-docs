<!-- tabs:start -->

#### **Documentation**

## 1. Visão Geral:

* **Objetivo Principal e Problema Resolvido:**
  O código apresentado implementa uma interface para gerenciar transportadoras associadas a um armazém. Ele permite visualizar, adicionar e excluir transportadoras, além de configurar propriedades específicas da interface, como campos ocultos, campos somente leitura e ações disponíveis. O objetivo principal é facilitar a manipulação e visualização de dados relacionados às transportadoras de forma eficiente e organizada.

* **Tecnologias Utilizadas:**
  - Delphi (Object Pascal) para desenvolvimento da aplicação.
  - Componentes visuais como `TcxGrid`, `TcxEditRepositoryButtonItem` e `TcxGridDBTableView` para a interface gráfica.
  - Serviços SOAP para integração com serviços externos.
  - Manipulação de dados com `DBClient` e `DataPacket`.

* **Forma do Componente:**
  - **Grid Display:**
    - **Colunas do Grid e seus Tipos:**
      - `code` (string): Código da transportadora.
      - `name` (string): Nome da transportadora.
    - **Ações do Grid e seus Efeitos:**
      - Adicionar (`ADD`): Permite adicionar uma nova transportadora.
      - Excluir (`DELETE`): Remove uma transportadora selecionada.

---

## 2. Descrição da Funcionalidade:

* **Ações Específicas:**
  - Adicionar uma transportadora.
  - Excluir uma transportadora.
  - Buscar transportadoras por código ou detalhes.

* **Componentes Principais:**
  - `TcxGridDBTableView`: Exibe os dados das transportadoras.
  - `TcxEditRepositoryButtonItem`: Botão para buscar detalhes de transportadoras.
  - `GridSettings`: Configurações do grid, como campos ocultos e ordem de exibição.

* **Tradução para Pseudo-código:**
  - Evento `OnEditValueChanged`: `se valor do campo editado mudar, então execute função`.
  - Evento `OnButtonClick` do botão de busca: `se botão clicado, então execute busca de transportadora`.
  - Evento `OnSetAccessMode`: `se modo de acesso mudar, então ajuste visibilidade e configurações`.

---

## 3. Lógica Operacional:

* **Fluxo de Execução:**
  1. Inicialização do componente com o construtor `Create`.
  2. Configuração das propriedades do grid, como campos ocultos e ações disponíveis.
  3. Interação do usuário com o grid (edição, busca, adição ou exclusão).
  4. Execução de funções específicas baseadas nos eventos disparados.

* **Dados Necessários:**
  - Código do armazém (`warehouseCode`).
  - Código e nome da transportadora.

---

## 4. Regras de Negócio:

* **Ações e Pré-condições:**
  - **Adicionar:** Disponível sempre.
  - **Excluir:** Disponível apenas se uma transportadora estiver selecionada.

* **Filtros Disponíveis:**
  - Não há filtros explícitos definidos no código.

* **Mensagens de Erro:**
  - Não há mensagens de erro explícitas definidas no código.

* **Valores Padrão dos Campos:**
  - Não há valores padrão explícitos definidos no código.

* **Validações e Condições dos Campos:**
  - Não há validações explícitas definidas no código.

---

## 5. Funções Principais:

* **Descrição das Funções:**
  - `m_FindCarrier`: Realiza a busca de uma transportadora.
  - `m_FindByCodeCarrier`: Busca uma transportadora pelo código.
  - `m_SetOnSetAccessMode`: Ajusta o modo de acesso e configurações do grid.

---

## 6. Consumo de Serviços API:

* **Chamadas a Serviços Externos:**
  - Serviço: `CarrierServiceUtils`.
  - Propósito: Buscar informações de transportadoras.
  - Dados enviados e recebidos não estão explicitamente definidos no código.

---

## 7. Campos Condicionais (Lógica do Formulário):

* Não há campos condicionais explícitos definidos no código.

---

## 8. Dependências:

* **Bibliotecas Externas:**
  - `SOAPHTTPClient`: Para integração com serviços SOAP.
  - `cxGrid`, `cxEditRepository`: Para componentes visuais.

* **Componentes Customizados:**
  - `TFRAMEBaseGridEditSOA`: Classe base para o frame.

---

## 9. Listagem de Campos e Validações:

* **Campos no Formulário:**
  - `code` (string): Não definido como obrigatório no código.
  - `name` (string): Não definido como obrigatório no código.

* **Mapeamento de Valores e Colunas do Banco de Dados:**
  - `code` → Coluna `code`.
  - `name` → Coluna `name`.

---

## 10. Exemplos e Diagramas:

* **Fluxograma:** Não aplicável.
* **Diagrama de Sequência:** Não aplicável.
* **Exemplo de Código:**
  ```pascal
  procedure TFRAMEwarehouseCarrier.m_FindCarrier(Sender: TObject; AButtonIndex: Integer);
  begin
    // Implementação da busca de transportadora
  end;
  ```
* **HTML Representando o Grid:**
  ```html
  <table style="width: 100%; border: 1px solid black;">
    <thead>
      <tr>
        <th style="border: 1px solid black;">Código</th>
        <th style="border: 1px solid black;">Nome</th>
      </tr>
    </thead>
    <tbody>
      <tr>
        <td style="border: 1px solid black;">001</td>
        <td style="border: 1px solid black;">Transportadora A</td>
      </tr>
      <tr>
        <td style="border: 1px solid black;">002</td>
        <td style="border: 1px solid black;">Transportadora B</td>
      </tr>
    </tbody>
  </table>
  ```

---

## 11. Comentários Importantes no Código:

* Configuração de propriedades do frame:
  ```pascal
  MasterKeyFields := 'warehouseCode';
  DataPacketName := 'Carrier';
  PropertyName := 'carriers';
  FrameType := frtDetail;
  ```

* Configuração de campos ocultos:
  ```pascal
  HiddenFields.Add('warehouseCode');
  HiddenFields.Add('updBy');
  HiddenFields.Add('lastUpd');
  ```

---

## 12. Conclusão:

O código implementa um frame para gerenciar transportadoras associadas a um armazém, com funcionalidades básicas de adição, exclusão e busca. Ele é bem estruturado e utiliza componentes visuais avançados, mas carece de validações explícitas e mensagens de erro. Sua integração com serviços SOAP é um ponto forte.

---

## 13. Resumo Curto:

O código implementa um frame para gerenciar transportadoras de armazéns, permitindo adicionar, excluir e buscar transportadoras. Ele utiliza componentes visuais avançados e integra-se a serviços SOAP para manipulação de dados.#### **FRwarehouseCarrier.pas**

```
unit FRwarehouseCarrier;

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
  TFRAMEwarehouseCarrier = class(TFRAMEBaseGridEditSOA)
    cxEDTfindCarrierDetail: TcxEditRepositoryButtonItem;
    procedure cxDBVtableEditValueChanged(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
    procedure ACTaddExecute(Sender: TObject);
    procedure cxDBVtableFocusedItemChanged(Sender: TcxCustomGridTableView;
      APrevFocusedItem, AFocusedItem: TcxCustomGridTableItem);
  private
    { Private declarations }
    procedure m_FindCarrier(Sender: TObject; AButtonIndex: Integer);
    procedure m_FindByCodeCarrier(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
    procedure m_SetOnSetAccessMode(Sender: TObject; var pv_State: Boolean);
    function m_FindByCodeCarriers(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem): Boolean;

  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
  end;

var
  FRAMEwarehouseCarrier: TFRAMEwarehouseCarrier;

implementation

uses
  kneFindDialogSOA, kneUtils, kneInterfaces, Global,
  CarrierServiceUtils, BaseServiceUtils, kneTypes,
  kneFindDialog, kneDialogFactory, kneFGFindUtils, kneFREditSOA;

{$R *.dfm}

constructor TFRAMEwarehouseCarrier.Create(AOwner: TComponent);
begin
  inherited;

  // SET DAS PROPRIEDADES DA FRAME
  MasterKeyFields := 'warehouseCode';
  DataPacketName := 'Carrier';                       // o nome do detail no datapacket(metadata) � sempre no singular
  PropertyName := 'carriers';                        // nome do campo da metadata que vai conter os details
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
    HiddenFields.Clear;
    HiddenFields.Add('warehouseCode');
    HiddenFields.Add('updBy');
    HiddenFields.Add('lastUpd');
    // Ordem Campos ............................................................
    OrderFields.Add('code');
    OrderFields.Add('name');
    // Key Fields ..............................................................
    KeyFields:= 'warehouseCode;code';
    // Custom Editors ..........................................................
    AddCustomField('code','cxEDTfindCarrierDetail');
  end; //with

  // Atribui��o dos eventos dos Finds
  cxEDTfindCarrierDetail.Properties.OnButtonClick := m_FindCarrier;
  OnSetAccessMode := m_SetOnSetAccessMode;
end;

procedure TFRAMEwarehouseCarrier.m_SetOnSetAccessMode(Sender: TObject; var pv_State: Boolean);
begin
//  if (not assigned(CDStable)) or (not CDStable.Active) then  //JAR 10-02-2010  Aqui N�o � necess�ria a protec��o, pois n�o acede ao datset
//    exit;
  SetColsWidthInGrid('80;200;', cxDBVtable);
end;


procedure TFRAMEwarehouseCarrier.m_FindByCodeCarrier(
  Sender: TcxCustomGridTableView; AItem: TcxCustomGridTableItem);
var
  lv_Service: TCarrierServiceUtils;
```

#### **FRwarehouseCarrier.dfm**

```
inherited FRAMEwarehouseCarrier: TFRAMEwarehouseCarrier
  inherited cxDBG: TcxGrid
    inherited cxDBVtable: TcxGridDBTableView
      OnEditValueChanged = cxDBVtableEditValueChanged
    end
  end
  inherited cxEDTR: TcxEditRepository
    object cxEDTfindCarrierDetail: TcxEditRepositoryButtonItem
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

