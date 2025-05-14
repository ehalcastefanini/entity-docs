<!-- tabs:start -->

#### **Documentation**

## 1. Visão Geral:

* **Objetivo Principal:**  
  O código implementa um componente de interface gráfica para gerenciar mercados associados a uma região de vendas. Ele permite adicionar, excluir e visualizar mercados vinculados a uma região específica. O objetivo é facilitar a manipulação de dados relacionados a mercados e regiões de vendas em um sistema.

* **Tecnologias Utilizadas:**  
  - **Delphi:** Linguagem de programação utilizada para criar a aplicação.
  - **Componentes cxGrid:** Para exibição e manipulação de dados em formato de tabela.
  - **SOAP (Simple Object Access Protocol):** Para comunicação com serviços externos.
  - **DBClient:** Para manipulação de dados em memória.

* **Forma do Componente:**  
  - **Exibição em Grade (Grid Display):**
    - **Colunas da Grade:**
      - `marketCd` (Código do Mercado) - Tipo: String.
      - `marketDesc` (Descrição do Mercado) - Tipo: String.
    - **Ações da Grade:**
      - **Adicionar:** Permite adicionar um novo mercado à região.
      - **Excluir:** Remove um mercado associado à região.

---

## 2. Descrição da Funcionalidade:

* **Ações Disponíveis:**
  - Adicionar um mercado à região.
  - Excluir um mercado da região.
  - Pesquisar mercados por código.

* **Componentes Principais:**
  - `cxGridDBTableView`: Exibe os dados em formato de tabela.
  - `cxEDTfindMarket`: Botão para buscar mercados.
  - `ACTaddExecute`: Ação para adicionar mercados.
  - `m_SetFindMktAdd`: Configura o comportamento do botão de busca.

* **Tradução para Pseudo-código:**
  - Evento `OnClick` do botão "Adicionar":  
    `if botão adicionar clicado then execute função adicionar mercado`.
  - Evento `OnEditValueChanged` na grade:  
    `if valor da célula alterado then valide e atualize o campo`.

---

## 3. Lógica Operacional:

* **Fluxo de Execução:**
  1. Inicialização do componente (`Create`):
     - Configurações iniciais, como campos principais, nome do pacote de dados e ações disponíveis.
  2. Interação do Usuário:
     - O usuário pode clicar no botão "Adicionar" para incluir um mercado ou editar diretamente os valores na grade.
  3. Funções Executadas:
     - `ACTaddExecute`: Adiciona um mercado.
     - `m_SetFindMktAdd`: Configura o botão de busca.

* **Dados Necessários:**
  - Código do Mercado (`marketCd`).
  - Descrição do Mercado (`marketDesc`).

---

## 4. Regras de Negócio:

* **Ações e Pré-condições:**
  - **Adicionar Mercado:** Requer que o código do mercado seja único e válido.
  - **Excluir Mercado:** Requer que um mercado esteja selecionado na grade.

* **Filtros Disponíveis:**
  - Filtro por código do mercado.

* **Mensagens de Erro:**
  - "Código do mercado já adicionado" se o mercado já estiver associado.
  - "Campo obrigatório não preenchido" se algum campo necessário estiver vazio.

* **Valores Padrão dos Campos:**
  - Não definidos explicitamente no código.

* **Validações e Condições dos Campos:**
  - `marketCd`: Deve ser único e não vazio.
  - `marketDesc`: Deve ser preenchido.

---

## 5. Funções Principais:

* **`Create`:**  
  Configura o componente, define campos principais e ações disponíveis.

* **`ACTaddExecute`:**  
  Adiciona um mercado à região.

* **`m_SetFindMktAdd`:**  
  Configura o comportamento do botão de busca.

* **`m_OnSetAccessMode`:**  
  Define o modo de acesso e configura a grade.

---

## 6. Consumo de Serviços API:

* **Serviço Externo:**  
  - **Nome do Serviço:** `CustomerMarketServiceUtils`.
  - **Finalidade:** Gerenciar mercados associados a regiões de vendas.

---

## 7. Campos Condicionais (Lógica do Formulário):

* Não há campos condicionais explícitos no código.

---

## 8. Dependências:

* **Bibliotecas Externas:**
  - `cxGrid`: Para exibição de dados em grade.
  - `SOAPHTTPClient`: Para comunicação com serviços SOAP.

* **Componentes Customizados:**
  - `kneFRGridEditSOA`: Classe base para edição de grades.

---

## 9. Listagem de Campos e Validações:

* **Campos:**
  - `marketCd` (Tipo: String, Obrigatório).
  - `marketDesc` (Tipo: String, Obrigatório).

* **Mapeamento de Valores:**
  - `marketCd` → Coluna `marketCd` no banco de dados.
  - `marketDesc` → Coluna `marketDesc` no banco de dados.

---

## 10. Exemplos e Diagramas:

* **Diagrama de Fluxo:**  
  Não aplicável.

* **Diagrama de Sequência:**  
  Não aplicável.

* **Exemplo de Código:**  
  ```delphi
  FRAMEsalesRegionMkt := TFRAMEsalesRegionMkt.Create(Self);
  FRAMEsalesRegionMkt.ACTaddExecute(Self);
  ```

* **HTML Representando a Grade:**
  ```html
  <table style="width: 100%; border: 1px solid black;">
    <thead>
      <tr>
        <th style="width: 100px;">Código do Mercado</th>
        <th style="width: 150px;">Descrição do Mercado</th>
      </tr>
    </thead>
    <tbody>
      <tr>
        <td>001</td>
        <td>Mercado A</td>
      </tr>
      <tr>
        <td>002</td>
        <td>Mercado B</td>
      </tr>
    </tbody>
  </table>
  ```

---

## 11. Comentários Importantes no Código:

* **Configuração Inicial:**  
  ```delphi
  MasterKeyFields := 'salesRegionCd';
  DataPacketName := 'SalesRegionMkt';
  PropertyName := 'salesRegionMkts';
  ```

* **Configuração da Grade:**  
  ```delphi
  DefineHiddenFields('HIDE_ALL_FIELDS');
  DefineOrderFields('marketCd;marketDesc');
  ```

---

## 12. Conclusão:

O código fornece uma interface eficiente para gerenciar mercados associados a regiões de vendas. Ele utiliza componentes visuais avançados e integrações com serviços SOAP. No entanto, faltam validações explícitas e mensagens de erro detalhadas, o que pode ser melhorado para maior robustez.

---

## 13. Resumo Curto:

O código implementa uma interface para gerenciar mercados associados a regiões de vendas, permitindo adicionar, excluir e visualizar mercados. Ele utiliza componentes visuais e serviços SOAP para manipulação de dados.#### **FRsalesRegionMkt.pas**

```
unit FRsalesRegionMkt;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFRGridEditSOA, cxStyles, cxCustomData, cxGraphics, cxFilter,
  cxData, cxDataStorage, cxEdit, DB, cxDBData, InvokeRegistry,
  cxEditRepositoryItems, sFrameAdapter, ImgList, ActnList, ExtCtrls, Rio,
  SOAPHTTPClient, DBClient, kneFRGridManager, StdCtrls, Buttons, sBitBtn,
  sPanel, cxGridLevel, cxClasses, cxControls, cxGridCustomView,
  cxGridCustomTableView, cxGridTableView, cxGridDBTableView, cxGrid,
  CustomerMarketsToAddRegionServiceUtils;

type
  TFRAMEsalesRegionMkt = class(TFRAMEBaseGridEditSOA)
    cxEDTfindMarket: TcxEditRepositoryButtonItem;
    procedure ACTaddExecute(Sender: TObject);
    procedure cxDBVtableEditValueChanged(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);

  private
    { Private declarations }

    mv_AlreadyAdded: String;

    procedure m_OnSetAccessMode(Sender: TObject; var pv_value: Boolean);
    function SetFieldRequiredState(const pv_Field: TField;
      pv_State: Boolean): Boolean;
    procedure m_SetFindMktAdd(Sender: TObject; AButtonIndex: Integer);
    procedure m_FindByCodeMarket(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
      function m_AlreadyAdded: string;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
  end;

var
  FRAMEsalesRegionMkt: TFRAMEsalesRegionMkt;

const
  gc_ColsWith =  '100;150;';

implementation

uses
  kneTypes, kneFGControlsUtils, Global, kneUtils, kneFindDialog,
  kneDialogFactory, kneFGFindUtils, kneFGDBUtils,
  // ServiceUtils
  CustomerMarketServiceUtils;

{$R *.dfm}

constructor TFRAMEsalesRegionMkt.Create(AOwner: TComponent);
begin
  inherited;

  // SET DAS PROPRIEDADES DA FRAME
  MasterKeyFields := 'salesRegionCd';
  DataPacketName := 'SalesRegionMkt';        // o nome do detail no datapacket(metadata) � sempre no singular
  PropertyName := 'salesRegionMkts';                  // nome do campo da metadata que vai conter os details
  FrameType := frtDetail;

  // configurar visibilidade de painel de ac��es e ac��es dispon�veis
  // pode ser usado em qualquer sitio do c�digo e a frame � imediatamente
  // redesenhada
  AvailableActions := 'add;delete';

  //Definir aqui os settings da dxGrid
  with GridSettings do
  begin
    DefineHiddenFields('HIDE_ALL_FIELDS');
    DefineOrderFields('marketCd;marketDesc');

    // Key Fields ..............................................................
//    KeyFields:= '';

    // Custom Editors ..........................................................
    AddCustomField('marketCd','cxEDTfindMarket');
    UseColsBestFit := False;
  end; //with

  OnSetAccessMode := m_OnSetAccessMode;
  cxEDTfindMarket.Properties.OnButtonClick := m_SetFindMktAdd;

  mv_AlreadyAdded := '';

end;

procedure TFRAMEsalesRegionMkt.m_OnSetAccessMode(Sender: TObject;var pv_value: Boolean);
begin
  inherited;

  SetColsWidthInGrid(gc_ColsWith, cxDBVtable);

  if (not assigned(CDStable)) or (not CDStable.Active) then
    exit;

//  CDStable.FieldByName('marketCd').ReadOnly := False;
```

#### **FRsalesRegionMkt.dfm**

```
inherited FRAMEsalesRegionMkt: TFRAMEsalesRegionMkt
  inherited cxDBG: TcxGrid
    inherited cxDBVtable: TcxGridDBTableView
      OnEditValueChanged = cxDBVtableEditValueChanged
    end
  end
  inherited cxEDTR: TcxEditRepository
    object cxEDTfindMarket: TcxEditRepositoryButtonItem
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
    end
  end
end
```
<!-- tabs:end -->

